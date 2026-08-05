open! Core
open Jsip_types

(* The dump is a stream of deltas: every node's definition appears once,
   under its wire id, and later occurrences are [Id] references — a shared
   block, a cycle, or a whole re-observed structure collapsed to a stub. This
   is the table those references resolve against, as of one step; a renderer
   expands a reference by looking its id up here. *)
module Nodes = struct
  type t = Snapshot.Node.t Int.Map.t

  let empty : t = Int.Map.empty

  let define t node =
    Snapshot.Node.fold node ~init:t ~f:(fun t (node : Snapshot.Node.t) ->
      match Snapshot.Node.is_revisit_stub node with
      (* a stub states nothing: it must not overwrite the definition *)
      | true -> t
      | false -> Map.set t ~key:node.id ~data:node)
  ;;

  let find t id = Map.find t id
end

module Visibility = struct
  type t =
    | In_scope
    | Shadowed
    | Out_of_scope
    | Unknown
  [@@deriving sexp_of, equal]

  (* [Unknown] reads as reachable: a dump that says nothing about scope, and
     a structure that never had a name to be reached by, are both cases where
     dimming would be an invention *)
  let is_reachable = function
    | In_scope | Unknown -> true
    | Shadowed | Out_of_scope -> false
  ;;
end

module Structure = struct
  type t =
    { id : int
    ; name : string option
    ; ty : Type_info.t option
    ; address : Snapshot.Address.t
    ; snapshot : Snapshot.t
    ; is_current : bool
    ; visibility : Visibility.t
    }

  (* the latest variable name, or [#id] for a structure never observed under
     one — the registry entry's own rule, which this is built from *)
  let display t = Registry_entry.display_name ~id:t.id ~name:t.name

  (* The walk's shape stamped with the registry's address of record: the
     registry re-captures every structure's address at every event, while the
     snapshot keeps whatever the structure's own last walk saw — so a redraw
     always shows the current root address even when the walk is older.
     Interior addresses stay as-of that walk; only the runtime can refresh
     those. *)
  let current_root t =
    { t.snapshot.root_node with virtual_address = t.address }
  ;;
end

module Step = struct
  type t =
    { call : Call.t
    ; frames : Call.t list
    ; structures : Structure.t list
    ; nodes : Nodes.t
    ; new_addresses : Snapshot.Address.Set.t
    }
end

type t =
  { steps : Step.t Array.t
  ; files : string list
  }

let addresses_of_snapshot (snapshot : Snapshot.t) =
  Snapshot.Node.fold
    snapshot.root_node
    ~init:Snapshot.Address.Set.empty
    ~f:(fun acc (node : Snapshot.Node.t) ->
      let acc = Set.add acc node.virtual_address in
      List.fold node.block ~init:acc ~f:(fun acc (_label, block) ->
        match block with
        | Address address -> Set.add acc address
        | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
        | Float_array _ | Id _ | Child ->
          acc))
;;

let description (call : Call.t) =
  let fn = Function_info.display call.info.function_info in
  let args =
    List.map call.info.arguments ~f:Argument.display
    |> String.concat ~sep:" "
  in
  let callee =
    match args with "" -> fn | args -> [%string "%{fn} %{args}"]
  in
  [%string "%{callee} \u{2014} %{Location.display call.info.location}"]
;;

let create call_stack =
  let seen = ref Snapshot.Address.Set.empty in
  (* each event walks one structure; everything else in the registry keeps
     the shape of its own most recent walk *)
  let latest_walk = ref Int.Map.empty in
  (* a structure's static type never changes, so its latest statement stands;
     a structure whose events all predate the wire's [ty] field simply has
     none *)
  let latest_ty = ref Int.Map.empty in
  (* which [let] each structure is bound by, from the events that said so —
     the registry's name plus this is what a scope is compared against *)
  let latest_binder = ref Int.Map.empty in
  (* every node definition the dump has stated so far *)
  let nodes = ref Nodes.empty in
  let steps =
    Array.init (Call_stack.length call_stack) ~f:(fun step ->
      let call = Call_stack.call_exn call_stack ~step in
      let addresses = addresses_of_snapshot call.info.snapshot in
      let new_addresses = Set.diff addresses !seen in
      seen := Set.union !seen addresses;
      nodes := Nodes.define !nodes call.info.snapshot.root_node;
      (* a re-observed structure's event is a stub — its shape is what its id
         was defined as earlier, wearing the address stated now *)
      let snapshot =
        let root = call.info.snapshot.root_node in
        match
          Snapshot.Node.is_revisit_stub root, Nodes.find !nodes root.id
        with
        | true, Some definition ->
          { call.info.snapshot with
            root_node =
              { definition with virtual_address = root.virtual_address }
          }
        | (true | false), (Some _ | None) -> call.info.snapshot
      in
      latest_walk := Map.set !latest_walk ~key:call.info.id ~data:snapshot;
      (match call.info.ty with
       | None -> ()
       | Some ty ->
         latest_ty := Map.set !latest_ty ~key:call.info.id ~data:ty);
      (match call.info.binder with
       | None -> ()
       | Some binder ->
         latest_binder
         := Map.set !latest_binder ~key:call.info.id ~data:binder);
      let frames = Call_stack.frames_at call_stack ~step in
      (* a name is reachable from anywhere on the stack, so the frames'
         scopes stack up outermost first — a structure the caller bound is
         still called what the caller called it, unless the frame we are in
         bound that name itself *)
      let scope =
        List.fold frames ~init:None ~f:(fun acc (frame : Call.t) ->
          match frame.info.scope with
          | None -> acc
          | Some inner ->
            let outer = Option.value acc ~default:Scope.empty in
            Some (Scope.nest ~outer ~inner))
      in
      (* the registry says what a structure is called; the scope says what
         that name now means. They part company when a [let] rebinds the name
         (the older version is shadowed) or when the binding's scope has been
         left behind altogether. *)
      let visibility ~id ~name =
        let known =
          let%bind.Option scope in
          let%bind.Option name in
          let%map.Option binder = Map.find !latest_binder id in
          scope, name, binder
        in
        match known with
        | None -> Visibility.Unknown
        | Some (scope, name, binder) ->
          (match Scope.binder scope ~name with
           | None -> Visibility.Out_of_scope
           | Some current ->
             (match Scope.Binder.equal current binder with
              | true -> Visibility.In_scope
              | false -> Visibility.Shadowed))
      in
      let structures =
        (* a registry id always has a walk by the time it appears — its first
           event is what registered it — so a miss is dropped rather than
           invented *)
        List.filter_map
          call.info.registry
          ~f:(fun { Registry_entry.id; address; name } ->
            Map.find !latest_walk id
            |> Option.map ~f:(fun snapshot ->
              { Structure.id
              ; name
              ; ty = Map.find !latest_ty id
              ; address
              ; snapshot
              ; is_current = id = call.info.id
              ; visibility = visibility ~id ~name
              }))
      in
      { Step.call; frames; structures; nodes = !nodes; new_addresses })
  in
  let files =
    Array.to_list steps
    |> List.map ~f:(fun (step : Step.t) ->
      Location.file_path step.call.info.location)
    |> List.stable_dedup ~compare:String.compare
  in
  { steps; files }
;;

let length t = Array.length t.steps
let step_exn t ~step = t.steps.(step)
let files t = t.files
