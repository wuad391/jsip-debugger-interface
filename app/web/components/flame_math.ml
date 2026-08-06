open! Core
open Jsip_types

module Path = struct
  type t = Flame_tree.Key.t list [@@deriving sexp_of, equal]
end

let live_path tree ~frames =
  List.map
    (Flame_tree.path tree ~frames)
    ~f:(fun (node : Flame_tree.Node.t) -> node.key)
;;

(* a drawn bar must be readable as a bar; anything thinner pools into the
   [+N] marker, which is floored wide enough to say how many it stands for *)
let min_child = 3.
let min_pool = 18.

module Segment = struct
  type t =
    | Bar of
        { path : Path.t
        ; label : string
        ; x : float
        ; width : float
        ; share : float option
        ; lit : bool
        ; deepest : bool
        }
    | Pool of
        { x : float
        ; width : float
        ; count : int
        }
  [@@deriving sexp_of]
end

module Row = struct
  type t =
    { depth : int
    ; segments : Segment.t list
    }
  [@@deriving sexp_of]
end

(* [width] px shared among a node's children (by inclusive weight, in the
   order given) and its own calls. From the first child too narrow to draw,
   the TAIL pools into one marker — pooling the middle would tear the row's
   tiling — and the marker's floor is paid for by scaling the drawn bars
   down, so the segments still tile the parent exactly. The parent's own
   share needs no floor: a gap is what self time looks like. *)
let apportion ~width ~self ~children =
  let total = Float.of_int (self + List.fold children ~init:0 ~f:( + )) in
  match Float.( <= ) total 0. || Float.( <= ) width 0. with
  | true -> [], None
  | false ->
    let scale = width /. total in
    let widths =
      List.map children ~f:(fun weight -> Float.of_int weight *. scale)
    in
    let drawn, pooled =
      match
        List.findi widths ~f:(fun (_ : int) w -> Float.( < ) w min_child)
      with
      | None -> widths, []
      | Some (at, (_ : float)) -> List.take widths at, List.drop widths at
    in
    (match pooled with
     | [] -> drawn, None
     | pooled ->
       let count = List.length pooled in
       let natural = List.fold pooled ~init:0. ~f:( +. ) in
       let pool = Float.min (Float.max natural min_pool) (width /. 2.) in
       let drawn_total = List.fold drawn ~init:0. ~f:( +. ) in
       let squeeze =
         match Float.( > ) drawn_total 0. with
         | false -> 1.
         | true ->
           Float.max 0. (drawn_total -. (pool -. natural)) /. drawn_total
       in
       List.map drawn ~f:(fun w -> w *. squeeze), Some (pool, count))
;;

let bars tree ~zoom ~width ~live =
  let rows = Int.Table.create () in
  let add depth segment =
    Hashtbl.update rows depth ~f:(fun segments ->
      segment :: Option.value segments ~default:[])
  in
  let has_profile =
    Map.exists
      tree.Flame_tree.functions
      ~f:(fun (metrics : Flame_tree.Metrics.t) ->
        Option.is_some metrics.share)
  in
  let share (node : Flame_tree.Node.t) =
    match has_profile with
    | true -> Flame_tree.prorated_share tree node
    | false ->
      (match tree.total_events with
       | 0 -> None
       | total -> Some (Float.of_int node.inclusive /. Float.of_int total))
  in
  (* [live] is the live path's remaining suffix: this node is lit when its
     key is the head, and its children then match against the rest *)
  let rec walk (node : Flame_tree.Node.t) ~rev_path ~x ~width ~depth ~live =
    let lit, rest =
      match (live : Path.t) with
      | key :: rest when Flame_tree.Key.equal key node.key -> true, rest
      | (_ : Flame_tree.Key.t) :: _ | [] -> false, []
    in
    add
      depth
      (Segment.Bar
         { path = List.rev (node.key :: rev_path)
         ; label = Flame_tree.Key.display node.key
         ; x
         ; width
         ; share = share node
         ; lit
         ; deepest = lit && List.is_empty rest
         });
    let drawn, pool =
      apportion
        ~width
        ~self:node.calls
        ~children:
          (List.map node.children ~f:(fun (child : Flame_tree.Node.t) ->
             child.inclusive))
    in
    let end_x =
      List.fold
        (List.zip_exn (List.take node.children (List.length drawn)) drawn)
        ~init:x
        ~f:(fun cx ((child : Flame_tree.Node.t), w) ->
          walk
            child
            ~rev_path:(node.key :: rev_path)
            ~x:cx
            ~width:w
            ~depth:(depth + 1)
            ~live:(match lit with true -> rest | false -> []);
          cx +. w)
    in
    match pool with
    | None -> ()
    | Some (pool_width, count) ->
      add (depth + 1) (Segment.Pool { x = end_x; width = pool_width; count })
  in
  (* a zoom that no longer resolves means the whole tree — the TUI's fallback *)
  let zoom =
    match (zoom : Path.t) with
    | [] -> []
    | path ->
      (match Flame_tree.find tree ~path with
       | Some (_ : Flame_tree.Node.t) -> path
       | None -> [])
  in
  (match zoom with
   | [] ->
     let drawn, (_ : (float * int) option) =
       apportion
         ~width
         ~self:0
         ~children:
           (List.map tree.roots ~f:(fun (node : Flame_tree.Node.t) ->
              node.inclusive))
     in
     ignore
       (List.fold
          (List.zip_exn (List.take tree.roots (List.length drawn)) drawn)
          ~init:0.
          ~f:(fun cx ((node : Flame_tree.Node.t), w) ->
            walk node ~rev_path:[] ~x:cx ~width:w ~depth:0 ~live;
            cx +. w)
        : float)
   | zoom ->
     (match Flame_tree.find tree ~path:zoom with
      | None -> ()
      | Some node ->
        let live =
          match
            List.is_prefix live ~prefix:zoom ~equal:Flame_tree.Key.equal
          with
          | true -> List.drop live (List.length zoom - 1)
          | false -> []
        in
        walk
          node
          ~rev_path:(List.rev (List.drop_last_exn zoom))
          ~x:0.
          ~width
          ~depth:0
          ~live));
  Hashtbl.to_alist rows
  |> List.map ~f:(fun (depth, segments) ->
    { Row.depth; segments = List.rev segments })
  |> List.sort ~compare:(fun (a : Row.t) b -> Int.compare a.depth b.depth)
;;

let heat_source tree =
  match
    Map.exists
      tree.Flame_tree.functions
      ~f:(fun (metrics : Flame_tree.Metrics.t) ->
        Option.is_some metrics.share)
  with
  | true -> `Compute
  | false -> `Calls
;;
