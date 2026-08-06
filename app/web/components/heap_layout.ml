open! Core

let key_id ({ structure_id; path } : Heap_scene.Fold_key.t) =
  [%string
    "%{structure_id#Int}:%{String.concat ~sep:\".\" (List.map path \
     ~f:Int.to_string)}"]
;;

module Box = struct
  type t =
    { x : float
    ; y : float
    ; w : float
    ; h : float
    }
  [@@deriving sexp_of, equal]

  let lerp a b ~amount =
    let at v w = v +. ((w -. v) *. amount) in
    { x = at a.x b.x; y = at a.y b.y; w = at a.w b.w; h = at a.h b.h }
  ;;
end

module Placed = struct
  type t =
    { id : string
    ; node : Heap_scene.Node.t
    ; parent : string option
    ; edge_label : string
    ; depth : int
    }
  [@@deriving sexp_of]
end

module Head = struct
  type t =
    { root : Heap_scene.Root.t
    ; x : float
    ; y : float
    }
end

module Tier_layout = struct
  type t =
    { placed : Placed.t list (** pre-order, parents before children *)
    ; pos : Box.t String.Map.t
    ; heads : Head.t list
    ; width : float
    ; height : float
    }
end

(* ── sizes ─────────────────────────────────────────────────────────────
   Text is measured by character count against the mockup's per-tier glyph
   widths — the canvas font is monospace, so counting is exact enough, and
   keeping measurement out of the DOM keeps every size expect-testable. *)

let label_width label = Float.of_int (String.length label)

let line_width (line : Heap_scene.Line.t) =
  label_width (Heap_scene.Line.text line)
;;

(* Boxes are sized for the text at ONE-TO-ONE, but the tiers they belong to
   arrive well under that ([stops]), so a box at its own tier is drawn at
   roughly half scale — 11px type landing at six. Everything here and the
   font sizes in the canvas widget are a fifth larger than the mockup's for
   that reason: the mockup was a picture at 100%, this is a viewport. *)
let size (node : Heap_scene.Node.t) ~tier =
  match node.kind with
  | Heap_scene.Kind.Nil ->
    (match tier with 0 -> 8., 7. | (_ : int) -> 44., 34.)
  | Heap_scene.Kind.Shared (_ : int) | Heap_scene.Kind.Block ->
    let lines_w =
      List.fold node.lines ~init:0. ~f:(fun widest line ->
        Float.max widest (line_width line))
    in
    let raw_w =
      List.fold node.raw ~init:0. ~f:(fun widest ((_ : string), value) ->
        Float.max widest (54. +. (label_width value *. 8.5) +. 18.))
    in
    let w1 = Float.min 460. ((label_width node.label *. 11.) +. 32.) in
    let w2 =
      Float.min
        520.
        ((Float.max (label_width node.label) (lines_w +. 8.) *. 10.5) +. 36.)
    in
    let h2 = 43. +. (Float.of_int (List.length node.lines) *. 22.) in
    let w3 = Float.min 560. (Float.max w2 raw_w) in
    let h3 = h2 +. 13. +. (Float.of_int (List.length node.raw) *. 18.) in
    (match tier with
     | 0 -> 9. +. (Float.of_int (Int.min node.words 7) *. 1.6), 7.
     | 1 -> w1, 34.
     | 2 -> w2, h2
     | (_ : int) -> w3, h3)
;;

(* ── the tree layout, per tier ───────────────────────────────────────── The
   mockup's: siblings side by side in field order, each subtree as wide as
   itself or its children, the parent centered over its spread; one row of
   boxes per depth, each row as tall as its tallest box; each structure under
   a header line, and the structures packed into a grid of [columns]. *)

let gaps ~tier =
  let pick a b c d = match tier with 0 -> a | 1 -> b | 2 -> c | _ -> d in
  (* Tighter than the mockup's, and much tighter than the boxes are big. Air
     between nodes buys nothing once the boxes carry borders and headers of
     their own — it just pushes the tree past the viewport, which costs zoom,
     which costs legibility. Spend the pixels on the boxes. *)
  ( pick 7. 16. 20. 24. (* gx: between siblings *)
  , pick 16. 34. 42. 50. (* gy: between depth rows *)
  , pick 10. 30. 34. 34. (* hdr: header line above a root *)
  , pick 26. 54. 64. 74. (* root gap: between structures *) )
;;

(* One structure's tree in its OWN coordinates — header line at [y = 0], the
   root box below it — so {!compute} can put it anywhere in the grid without
   the recursion knowing where that is. *)
module Placement = struct
  type t =
    { placed : Placed.t list
    ; pos : Box.t String.Map.t
    ; root : Heap_scene.Root.t
    ; width : float
    ; height : float
    }
end

let layout_root (root : Heap_scene.Root.t) ~tier =
  let gx, gy, hdr, (_ : float) = gaps ~tier in
  let pos = ref String.Map.empty in
  let placed = Queue.create () in
  let max_x = ref 0. in
  (* row heights per depth, so a deep box never overlaps the row below *)
  let row_heights = Int.Table.create () in
  let rec measure (node : Heap_scene.Node.t) ~depth =
    let (_ : float), h = size node ~tier in
    Hashtbl.update row_heights depth ~f:(fun tallest ->
      Float.max h (Option.value tallest ~default:0.));
    List.iter node.children ~f:(fun ((_ : string), child) ->
      measure child ~depth:(depth + 1))
  in
  measure root.node ~depth:0;
  let depths =
    1 + (Hashtbl.keys row_heights |> List.fold ~init:0 ~f:Int.max)
  in
  let row_y = Array.create ~len:depths 0. in
  let acc = ref 0. in
  for depth = 0 to depths - 1 do
    row_y.(depth) <- !acc;
    acc
    := !acc
       +. Option.value (Hashtbl.find row_heights depth) ~default:0.
       +. gy
  done;
  let rec subtree_width (node : Heap_scene.Node.t) =
    let w, (_ : float) = size node ~tier in
    match node.children with
    | [] -> w
    | children ->
      let spread =
        List.fold children ~init:0. ~f:(fun total ((_ : string), child) ->
          total +. subtree_width child)
        +. (gx *. Float.of_int (List.length children - 1))
      in
      Float.max w spread
  in
  let rec place (node : Heap_scene.Node.t) ~left ~depth ~parent ~edge_label =
    let w, h = size node ~tier in
    let width = subtree_width node in
    let x = left +. ((width -. w) /. 2.) in
    let y = hdr +. row_y.(depth) in
    let id = key_id node.key in
    pos := Map.set !pos ~key:id ~data:{ Box.x; y; w; h };
    Queue.enqueue placed { Placed.id; node; parent; edge_label; depth };
    max_x := Float.max !max_x (left +. width);
    let child_x = ref (left +. ((width -. subtree_spread node) /. 2.)) in
    List.iter node.children ~f:(fun (label, child) ->
      let child_width = subtree_width child in
      place
        child
        ~left:!child_x
        ~depth:(depth + 1)
        ~parent:(Some id)
        ~edge_label:label;
      child_x := !child_x +. child_width +. gx)
  and subtree_spread (node : Heap_scene.Node.t) =
    match node.children with
    | [] -> subtree_width node
    | children ->
      List.fold children ~init:0. ~f:(fun total ((_ : string), child) ->
        total +. subtree_width child)
      +. (gx *. Float.of_int (List.length children - 1))
  in
  place root.node ~left:0. ~depth:0 ~parent:None ~edge_label:"";
  { Placement.placed = Queue.to_list placed
  ; pos = !pos
  ; root
  ; width = !max_x
  ; height = hdr +. !acc
  }
;;

let empty_layout =
  { Tier_layout.placed = []
  ; pos = String.Map.empty
  ; heads = []
  ; width = 0.
  ; height = 0.
  }
;;

let compute (roots : Heap_scene.Root.t list) ~tier ~columns =
  let (_ : float), (_ : float), (_ : float), root_gap = gaps ~tier in
  (* structures sit further apart across than down: two trees side by side
     have nothing between them but air, where stacked ones are separated by a
     header line as well *)
  let column_gap = root_gap *. 1.5 in
  let columns = Int.max 1 columns in
  match List.map roots ~f:(layout_root ~tier) with
  | [] -> empty_layout
  | placements ->
    let rows = (List.length placements + columns - 1) / columns in
    let column_w = Array.create ~len:columns 0. in
    let row_h = Array.create ~len:rows 0. in
    List.iteri placements ~f:(fun index (placement : Placement.t) ->
      let column = index % columns in
      let row = index / columns in
      column_w.(column) <- Float.max column_w.(column) placement.width;
      row_h.(row) <- Float.max row_h.(row) placement.height);
    let column_x = Array.create ~len:columns 0. in
    for column = 1 to columns - 1 do
      column_x.(column)
      <- column_x.(column - 1) +. column_w.(column - 1) +. column_gap
    done;
    let row_y = Array.create ~len:rows 0. in
    for row = 1 to rows - 1 do
      row_y.(row) <- row_y.(row - 1) +. row_h.(row - 1) +. root_gap
    done;
    let pos = ref String.Map.empty in
    let placed = Queue.create () in
    let heads = Queue.create () in
    let width = ref 0. in
    let height = ref 0. in
    List.iteri placements ~f:(fun index (placement : Placement.t) ->
      let dx = column_x.(index % columns) in
      let dy = row_y.(index / columns) in
      Map.iteri placement.pos ~f:(fun ~key ~data:(box : Box.t) ->
        pos
        := Map.set
             !pos
             ~key
             ~data:{ box with x = box.x +. dx; y = box.y +. dy });
      List.iter placement.placed ~f:(fun placed_node ->
        Queue.enqueue placed placed_node);
      Queue.enqueue heads { Head.root = placement.root; x = dx; y = dy };
      (* the extent is measured off the structures themselves, so trailing
         empty columns cost nothing *)
      width := Float.max !width (dx +. placement.width);
      height := Float.max !height (dy +. placement.height));
    { Tier_layout.placed = Queue.to_list placed
    ; pos = !pos
    ; heads = Queue.to_list heads
    ; width = !width
    ; height = !height
    }
;;

let all roots ~columns =
  Array.init 4 ~f:(fun tier -> compute roots ~tier ~columns)
;;

(* ── zoom plumbing ─────────────────────────────────────────────────────
   Semantic zoom: the scale factor [k] maps onto a continuous detail tier
   through log-spaced stops, geometry crossfades between the two neighbouring
   tiers' layouts, and the drawn CONTENT switches only once the box has
   nearly finished growing — so text never draws into a box that cannot hold
   it. *)

(* Where each detail tier begins, as scale factors. Every one of them is
   lower than it looks like it should be, because the boxes underneath are
   drawn at a size that reads at HALF scale: the machine-word tier is fully
   grown by 90%, and a box carrying its fields arrives at half that. Deep
   detail should not cost deep zoom. *)
let stops = [| 0.12; 0.28; 0.5; 0.9 |]

let tier_for ~k =
  let lk = Float.log k in
  match Float.( <= ) lk (Float.log stops.(0)) with
  | true -> 0.
  | false ->
    let rec walk i =
      match i < 3 with
      | false -> 3.
      | true ->
        let a = Float.log stops.(i) in
        let b = Float.log stops.(i + 1) in
        (match Float.( < ) lk b with
         | true -> Float.of_int i +. ((lk -. a) /. (b -. a))
         | false -> walk (i + 1))
    in
    walk 0
;;

let k_for_tier tier =
  let clamped = Float.clamp_exn tier ~min:0. ~max:3. in
  let i = Int.min 2 (Int.of_float clamped) in
  let a = Float.log stops.(i) in
  let b = Float.log stops.(i + 1) in
  Float.exp (a +. ((clamped -. Float.of_int i) *. (b -. a)))
;;

(* geometry reaches the next tier's size at [grow] of the octave, so the box
   is always fully grown before the content switches to that tier *)
let grow = 0.85

module Split = struct
  type t =
    { a : int
    ; b : int
    ; f : float
    }
end

let split tier_f =
  let clamped = Float.clamp_exn tier_f ~min:0. ~max:3. in
  let a = Int.of_float clamped in
  let b = Int.min 3 (a + 1) in
  { Split.a; b; f = Float.min 1. ((clamped -. Float.of_int a) /. grow) }
;;

let content_tier tier_f =
  let clamped = Float.clamp_exn tier_f ~min:0. ~max:3. in
  let a = Int.of_float clamped in
  match Float.( >= ) (clamped -. Float.of_int a) grow with
  | true -> Int.min 3 (a + 1)
  | false -> a
;;

let box_of (layouts : Tier_layout.t array) ~tier_f ~id =
  let { Split.a; b; f } = split tier_f in
  match Map.find layouts.(a).pos id with
  | None -> None
  | Some box_a ->
    (match a = b with
     | true -> Some box_a
     | false ->
       (match Map.find layouts.(b).pos id with
        | None -> Some box_a
        | Some box_b -> Some (Box.lerp box_a box_b ~amount:f)))
;;

let heads_now (layouts : Tier_layout.t array) ~tier_f =
  let { Split.a; b; f } = split tier_f in
  List.mapi layouts.(a).heads ~f:(fun index (head : Head.t) ->
    let other =
      Option.value (List.nth layouts.(b).heads index) ~default:head
    in
    { head with
      x = head.x +. ((other.x -. head.x) *. f)
    ; y = head.y +. ((other.y -. head.y) *. f)
    })
;;

let bounds_now (layouts : Tier_layout.t array) ~tier_f =
  let { Split.a; b; f } = split tier_f in
  let at v w = v +. ((w -. v) *. f) in
  ( at layouts.(a).width layouts.(b).width
  , at layouts.(a).height layouts.(b).height )
;;
