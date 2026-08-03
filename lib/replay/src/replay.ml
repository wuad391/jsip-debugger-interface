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

  let rec define t (node : Snapshot.Node.t) =
    let t =
      match Snapshot.Node.is_revisit_stub node with
      (* a stub states nothing: it must not overwrite the definition *)
      | true -> t
      | false -> Map.set t ~key:node.id ~data:node
    in
    List.fold node.children ~init:t ~f:define
  ;;

  let find t id = Map.find t id
end

module Structure = struct
  type t =
    { id : int
    ; name : string option
    ; ty : Type_info.t option
    ; address : Snapshot.Address.t
    ; snapshot : Snapshot.t
    ; is_current : bool
    }

  (* the latest variable name, or [#id] for a structure never observed under
     one *)
  let display t =
    match t.name with Some name -> name | None -> [%string "#%{t.id#Int}"]
  ;;

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
    ; description : string
    }
end

type t =
  { steps : Step.t Array.t
  ; files : string list
  }

let addresses_of_snapshot (snapshot : Snapshot.t) =
  let rec walk (node : Snapshot.Node.t) acc =
    let acc = Set.add acc node.virtual_address in
    let acc =
      List.fold node.block ~init:acc ~f:(fun acc (_label, block) ->
        match block with
        | Address address -> Set.add acc address
        | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
        | Float_array _ | Id _ ->
          acc)
    in
    List.fold node.children ~init:acc ~f:(fun acc child -> walk child acc)
  in
  walk snapshot.root_node Snapshot.Address.Set.empty
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
              }))
      in
      { Step.call
      ; frames = Call_stack.frames_at call_stack ~step
      ; structures
      ; nodes = !nodes
      ; new_addresses
      ; description = description call
      })
  in
  let files =
    Array.fold steps ~init:[] ~f:(fun acc (step : Step.t) ->
      let file = Location.file_path step.call.info.location in
      match List.mem acc file ~equal:String.equal with
      | true -> acc
      | false -> file :: acc)
    |> List.rev
  in
  { steps; files }
;;

let length t = Array.length t.steps
let step_exn t ~step = t.steps.(step)
let files t = t.files
