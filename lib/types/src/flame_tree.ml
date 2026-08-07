open! Core

module Key = struct
  module T = struct
    type t =
      | Named of string
      | Lambda of
          { text : string
          ; site : Location.t
          }
    [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make_plain (T)

  let of_call (call : Call.t) =
    let info = call.info in
    match info.function_info with
    | Function_name name -> Named name
    | Unnamed text -> Lambda { text; site = info.location }
  ;;

  let display t =
    match t with Named name -> name | Lambda { text; site = _ } -> text
  ;;
end

module Metrics = struct
  type t =
    { calls : int
    ; share : float option
    ; cost_per_call : float option
    ; relative_cost : float option
    }
  [@@deriving sexp_of]

  let none ~calls =
    { calls; share = None; cost_per_call = None; relative_cost = None }
  ;;
end

module Node = struct
  type t =
    { key : Key.t
    ; inclusive : int
    ; calls : int
    ; first_step : int
    ; children : t list
    }
  [@@deriving sexp_of]
end

type t =
  { roots : Node.t list
  ; total_events : int
  ; functions : Metrics.t Key.Map.t
  }
[@@deriving sexp_of]

(* the dynamic call forest, before identical paths pool *)
module Dyn = struct
  type t =
    { key : Key.t
    ; lo : int (* [fst range] *)
    ; step : int (* [snd range] — the call's own event *)
    ; children : t list (* chronological *)
    }
end

(* {!Call_stack.create}'s pending ranges tile [0, step - 1] in contiguous
   blocks, ascending bottom-to-top, so the subtrees inside this call are
   exactly the suffix whose [lo] has not fallen below its own — an [Int]
   comparison rather than a containment test, and O(1) per pop. Depth is
   deliberately unused: dumps have depth gaps, and [range] is {!Call.t}'s
   documented contract. *)
let forest ~(calls : Call.t array) =
  let pending = Stack.create () in
  Array.iteri calls ~f:(fun step (call : Call.t) ->
    let lo, (_ : int) = call.range in
    (* popping is top-first, so consing rebuilds chronological order *)
    let rec adopt children =
      match Stack.top pending with
      | Some (child : Dyn.t) when child.lo >= lo ->
        ignore (Stack.pop_exn pending : Dyn.t);
        adopt (child :: children)
      | Some (_ : Dyn.t) | None -> children
    in
    let children = adopt [] in
    Stack.push pending { Dyn.key = Key.of_call call; lo; step; children });
  List.rev (Stack.to_list pending)
;;

(* siblings go in name order, which is what a flame graph means by its
   x-axis: it maximizes merging and, more usefully, it makes the picture a
   function of the program rather than of the run — two captures of the same
   code lay out identically and can be read side by side. Sorting by width
   would put the heaviest subtree leftmost, but then every shift in the
   profile reshuffles the graph. Ties fall back to [Key.compare], so the
   order is total and a rendering of the tree is an expect test. *)
let by_name (a : Node.t) (b : Node.t) =
  match String.compare (Key.display a.key) (Key.display b.key) with
  | 0 -> Key.compare a.key b.key
  | ordering -> ordering
;;

module Group = struct
  type t =
    { inclusive : int
    ; calls : int
    ; first_step : int
    ; children : Dyn.t list list (* reversed; concatenated once *)
    }
end

let rec merge (dyns : Dyn.t list) =
  let grouped =
    List.fold dyns ~init:Key.Map.empty ~f:(fun grouped (dyn : Dyn.t) ->
      let inclusive = dyn.step - dyn.lo + 1 in
      Map.update grouped dyn.key ~f:(fun existing ->
        match existing with
        | None ->
          { Group.inclusive
          ; calls = 1
          ; first_step = dyn.step
          ; children = [ dyn.children ]
          }
        | Some (group : Group.t) ->
          { Group.inclusive = group.inclusive + inclusive
          ; calls = group.calls + 1
          ; first_step = Int.min group.first_step dyn.step
          ; children = dyn.children :: group.children
          }))
  in
  Map.to_alist grouped
  |> List.map ~f:(fun (key, (group : Group.t)) ->
    { Node.key
    ; inclusive = group.inclusive
    ; calls = group.calls
    ; first_step = group.first_step
    ; children = merge (List.concat (List.rev group.children))
    })
  |> List.sort ~compare:by_name
;;

(* one [Heat_profile.share] per distinct function rather than per event —
   [share] scans the profile's entries, so keying the fold first is what
   keeps this linear in the dump *)
let metrics_table ~(calls : Call.t array) ~profile =
  let counts =
    Array.fold calls ~init:Key.Map.empty ~f:(fun counts (call : Call.t) ->
      Map.update counts (Key.of_call call) ~f:(fun existing ->
        match existing with
        | None -> 1, call.info
        | Some (count, info) -> count + 1, info))
  in
  let shares =
    Map.map counts ~f:(fun (count, (info : Call.Info.t)) ->
      ( count
      , Option.bind profile ~f:(fun profile ->
          match
            Heat_profile.share
              profile
              ~function_info:info.function_info
              ~location:info.location
          with
          | Some share -> Some share
          (* what the call's FILE cost, when the profile cannot name the
             function itself. Without this a drawer full of instrumented
             expressions — which is what a dump of bindings inside functions
             is — comes back entirely unmatched the moment a profile is
             loaded, and every bar draws in the neutral "no data" color: the
             flame got LESS informative for having a profile. *)
          | None -> Heat_profile.file_share profile ~location:info.location)
      ))
  in
  (* the cost of an average sampled call, over the matched functions only: an
     unmatched one has no share to contribute and must not dilute the
     denominator either *)
  let sampled_share, sampled_calls =
    Map.fold shares ~init:(0., 0) ~f:(fun ~key:(_ : Key.t) ~data acc ->
      let share_total, call_total = acc in
      match data with
      | count, Some share -> share_total +. share, call_total + count
      | (_ : int), None -> share_total, call_total)
  in
  let average =
    match sampled_calls with
    | 0 -> None
    | (_ : int) ->
      let average = sampled_share /. Float.of_int sampled_calls in
      (match Float.( > ) average 0. with
       | true -> Some average
       | false -> None)
  in
  Map.map shares ~f:(fun (count, share) ->
    let cost_per_call =
      Option.map share ~f:(fun share -> share /. Float.of_int count)
    in
    { Metrics.calls = count
    ; share
    ; cost_per_call
    ; relative_cost = Option.map2 cost_per_call average ~f:( /. )
    })
;;

let create ~calls ~profile =
  { roots = merge (forest ~calls)
  ; total_events = Array.length calls
  ; functions = metrics_table ~calls ~profile
  }
;;

let metrics t (node : Node.t) =
  match Map.find t.functions node.key with
  | Some metrics -> metrics
  | None -> Metrics.none ~calls:node.calls
;;

let prorated_share t (node : Node.t) =
  let metrics = metrics t node in
  match metrics.calls with
  | 0 -> None
  | (_ : int) ->
    Option.map metrics.share ~f:(fun share ->
      share *. Float.of_int node.calls /. Float.of_int metrics.calls)
;;

let child_named nodes key =
  List.find nodes ~f:(fun (node : Node.t) -> Key.equal node.key key)
;;

let path t ~frames =
  let rec walk nodes frames found =
    match frames with
    | [] -> List.rev found
    | (frame : Call.t) :: rest ->
      (match child_named nodes (Key.of_call frame) with
       | None -> List.rev found
       | Some (node : Node.t) -> walk node.children rest (node :: found))
  in
  walk t.roots frames []
;;

let find t ~path =
  let rec walk nodes path =
    match path with
    | [] -> None
    | [ key ] -> child_named nodes key
    | key :: rest ->
      (match child_named nodes key with
       | None -> None
       | Some (node : Node.t) -> walk node.children rest)
  in
  walk t.roots path
;;

let by_cost_per_call t =
  Map.to_alist t.functions
  |> List.filter ~f:(fun ((_ : Key.t), (metrics : Metrics.t)) ->
    Option.is_some metrics.cost_per_call)
  |> List.sort ~compare:(fun (key, (a : Metrics.t)) (other, b) ->
    match
      Option.compare Float.descending a.cost_per_call b.cost_per_call
    with
    | 0 -> Key.compare key other
    | ordering -> ordering)
;;
