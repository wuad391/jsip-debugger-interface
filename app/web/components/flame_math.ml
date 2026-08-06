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

(* What fits ON a bar, which is a different question from what a call is
   called. An unnamed callee's name is its whole source expression — a record
   literal spanning six lines, in a bar forty pixels wide — so a lambda is
   labeled by WHERE it is instead: [order_book.ml:134] identifies it, sorts
   with its neighbours, and survives being cut off. The full text is still on
   the bar's tooltip, which is where a long thing belongs. *)
let bar_label (key : Flame_tree.Key.t) =
  match key with
  | Named name -> name
  | Lambda { text = (_ : string); site } ->
    [%string
      "%{Filename.basename (Location.file_path \
       site)}:%{Location.line_number site#Int}"]
;;

module Segment = struct
  type t =
    | Bar of
        { path : Path.t
        ; label : string (** what fits on the bar — see {!bar_label} *)
        ; detail : string
        (** the callee in full, for the tooltip: a lambda's whole source
            expression, which is what the bar cannot hold *)
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

(* [width] px shared among a node's children (by inclusive weight) and its
   own calls. Every child too narrow to read pools into one marker at the end
   of the row, and the marker's floor is paid for by scaling the drawn bars
   down, so the segments still tile the parent exactly. The parent's own
   share needs no floor: a gap is what self time looks like.

   The narrow ones pool WHEREVER they sit, not "everything after the first
   narrow one". Siblings are in name order, so a tail rule threw away every
   subtree that happened to sort after a thin one — on a real trace that
   pooled the widest bar on the row and left the drawer looking like it held
   a single call. The drawn bars keep their relative order, so the picture is
   still a function of the program rather than of the run.

   Results are [(index, width)] into [children]: the caller needs to know
   WHICH children survived, now that they are no longer a prefix. *)
let apportion ~width ~self ~children =
  let total = Float.of_int (self + List.fold children ~init:0 ~f:( + )) in
  match Float.( <= ) total 0. || Float.( <= ) width 0. with
  | true -> [], None
  | false ->
    let scale = width /. total in
    let widths =
      List.mapi children ~f:(fun index weight ->
        index, Float.of_int weight *. scale)
    in
    let drawn, pooled =
      List.partition_tf widths ~f:(fun ((_ : int), w) ->
        Float.( >= ) w min_child)
    in
    (match pooled with
     | [] -> drawn, None
     | pooled ->
       let count = List.length pooled in
       let natural =
         List.fold pooled ~init:0. ~f:(fun total ((_ : int), w) ->
           total +. w)
       in
       let pool = Float.min (Float.max natural min_pool) (width /. 2.) in
       let drawn_total =
         List.fold drawn ~init:0. ~f:(fun total ((_ : int), w) -> total +. w)
       in
       let squeeze =
         match Float.( > ) drawn_total 0. with
         | false -> 1.
         | true ->
           Float.max 0. (drawn_total -. (pool -. natural)) /. drawn_total
       in
       ( List.map drawn ~f:(fun (index, w) -> index, w *. squeeze)
       , Some (pool, count) ))
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
         ; label = bar_label node.key
         ; detail =
             String.concat_map
               (Flame_tree.Key.display node.key)
               ~f:(fun char ->
                 match Char.is_whitespace char with
                 | true -> " "
                 | false -> Char.to_string char)
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
    let children = Array.of_list node.children in
    let end_x =
      List.fold drawn ~init:x ~f:(fun cx (index, w) ->
        walk
          children.(index)
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
     let roots = Array.of_list tree.roots in
     ignore
       (List.fold drawn ~init:0. ~f:(fun cx (index, w) ->
          walk roots.(index) ~rev_path:[] ~x:cx ~width:w ~depth:0 ~live;
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
