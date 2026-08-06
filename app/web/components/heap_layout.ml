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

let size (node : Heap_scene.Node.t) ~tier =
  match node.kind with
  | Heap_scene.Kind.Nil ->
    (match tier with 0 -> 8., 7. | (_ : int) -> 30., 23.)
  | Heap_scene.Kind.Shared (_ : int) | Heap_scene.Kind.Block ->
    let lines_w =
      List.fold node.lines ~init:0. ~f:(fun widest line ->
        Float.max widest (line_width line))
    in
    let raw_w =
      List.fold node.raw ~init:0. ~f:(fun widest ((_ : string), value) ->
        Float.max widest (40. +. (label_width value *. 6.1) +. 14.))
    in
    let w1 = Float.min 320. ((label_width node.label *. 7.4) +. 20.) in
    let w2 =
      Float.min
        340.
        ((Float.max (label_width node.label) (lines_w +. 8.) *. 7.1) +. 24.)
    in
    let h2 = 24. +. (Float.of_int (List.length node.lines) *. 15.) in
    let w3 = Float.min 380. (Float.max w2 raw_w) in
    let h3 = h2 +. 9. +. (Float.of_int (List.length node.raw) *. 13.) in
    (match tier with
     | 0 -> 9. +. (Float.of_int (Int.min node.words 7) *. 1.6), 7.
     | 1 -> w1, 23.
     | 2 -> w2, h2
     | (_ : int) -> w3, h3)
;;

(* ── the tree layout, per tier ───────────────────────────────────────── The
   mockup's: siblings side by side in field order, each subtree as wide as
   itself or its children, the parent centered over its spread; one row of
   boxes per depth, each row as tall as its tallest box; roots stacked down
   the canvas, each under a header line. *)

let gaps ~tier =
  let pick a b c d = match tier with 0 -> a | 1 -> b | 2 -> c | _ -> d in
  ( pick 7. 24. 30. 38. (* gx: between siblings *)
  , pick 16. 54. 66. 78. (* gy: between depth rows *)
  , pick 10. 30. 34. 34. (* hdr: header line above a root *)
  , pick 26. 80. 96. 110. (* root gap: between structures *) )
;;

let compute (roots : Heap_scene.Root.t list) ~tier =
  let gx, gy, hdr, root_gap = gaps ~tier in
  let pos = ref String.Map.empty in
  let placed = Queue.create () in
  let max_x = ref 0. in
  let cy = ref 0. in
  let heads = Queue.create () in
  List.iter roots ~f:(fun root ->
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
    let rec place (node : Heap_scene.Node.t) ~left ~depth ~parent ~edge_label
      =
      let w, h = size node ~tier in
      let width = subtree_width node in
      let x = left +. ((width -. w) /. 2.) in
      let y = !cy +. hdr +. row_y.(depth) in
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
    Queue.enqueue heads { Head.root; y = !cy };
    cy := !cy +. hdr +. !acc +. root_gap);
  { Tier_layout.placed = Queue.to_list placed
  ; pos = !pos
  ; heads = Queue.to_list heads
  ; width = !max_x
  ; height = !cy
  }
;;

let all roots = Array.init 4 ~f:(fun tier -> compute roots ~tier)

(* ── zoom plumbing ─────────────────────────────────────────────────────
   Semantic zoom: the scale factor [k] maps onto a continuous detail tier
   through log-spaced stops, geometry crossfades between the two neighbouring
   tiers' layouts, and the drawn CONTENT switches only once the box has
   nearly finished growing — so text never draws into a box that cannot hold
   it. *)

(* where each detail tier begins, as scale factors: labels arrive well under
   1:1, fields around half scale, and the machine-word tier is fully grown by
   150% — deep detail should not cost deep zoom *)
let stops = [| 0.2; 0.45; 0.85; 1.5 |]

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
    { head with y = head.y +. ((other.y -. head.y) *. f) })
;;

let bounds_now (layouts : Tier_layout.t array) ~tier_f =
  let { Split.a; b; f } = split tier_f in
  let at v w = v +. ((w -. v) *. f) in
  ( at layouts.(a).width layouts.(b).width
  , at layouts.(a).height layouts.(b).height )
;;
