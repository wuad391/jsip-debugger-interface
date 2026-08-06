open! Core
open Jsip_types
open Jsip_web_components
module Vdom = Virtual_dom.Vdom
module Js = Js_of_ocaml.Js
module Dom = Js_of_ocaml.Dom
module Dom_html = Js_of_ocaml.Dom_html

module Input = struct
  type t =
    { roots : Heap_scene.Root.t list
    ; layouts : Heap_layout.Tier_layout.t array
    ; step : int
    ; land_seq : int
    (** bumped by stepping: the cue to bring the walked structure back into
        view if it slipped off *)
    ; focus_seq : int
    (** bumped by [.] and the header's [⌖]: zoom to whichever mark
        {!focus_target} names, wherever the view was *)
    ; focus_target : Action.Focus_target.t
    ; pulse_ids : string list
    (** boxes allocated at this step — ringed as the step arrives *)
    ; columns : int
    (** structures side by side before the next row of them — the corner
        slider's value; a change refits, since the whole arrangement moved *)
    ; current_root_id : string option
    ; current_structure_id : int option
    ; selected_address : Snapshot.Address.t option
    ; filter_active : bool
    ; lod : Action.Lod.t
    ; edge_style : Action.Edge_style.t
    ; typing_filter : bool
    ; theme : Theme.t
    ; inject : Action.t -> unit Vdom.Effect.t
    }
end

module Node_style = struct
  type t =
    { fill : string
    ; stroke : string
    ; ink : string
    ; dashed : bool
    }
end

module View_state = struct
  type t =
    { mutable x : float
    ; mutable y : float
    ; mutable k : float
    ; mutable k_target : float
    }
end

module Pulse = struct
  type t =
    { id : string
    ; started : float
    }
end

module Anchor = struct
  type t =
    | Node of
        { id : string
        ; nx : float
        ; ny : float
        ; sx : float
        ; sy : float
        }
    | Free of
        { wx : float
        ; wy : float
        ; sx : float
        ; sy : float
        }
end

module Mini = struct
  type t =
    { x : float
    ; y : float
    ; w : float
    ; h : float
    ; scale : float
    }
end

module State = struct
  type t =
    { mutable input : Input.t
    ; canvas : Dom_html.canvasElement Js.t
    ; context : Dom_html.canvasRenderingContext2D Js.t
    ; mutable dpr : float
    ; mutable width : float
    ; mutable height : float
    ; view : View_state.t
    ; mutable tier_f : float
    ; mutable anchor : Anchor.t option
    ; mutable drag : (float * float * float * float) option
    (** pointer-down client x/y and the view origin then *)
    ; mutable drag_moved : bool
    ; mutable mini_drag : bool
    ; mutable cursor : (float * float) option
    ; mutable hover : string option
    ; mutable pulses : Pulse.t list
    ; mutable styles : Node_style.t String.Table.t
    ; mutable dirty : bool
    ; mutable mini : Mini.t option
    ; mutable raf : Dom_html.animation_frame_request_id option
    ; mutable observer :
        Js_of_ocaml.ResizeObserver.resizeObserver Js.t option
    ; mutable keydown : Dom.event_listener_id option
    ; mutable hud_zoom : int
    ; mutable hud_tier : float
    ; mutable fitted : bool
    }
end

(* the wheel stops well past full detail (tier 3 is fully grown at 0.9×):
   past that, zoom only magnifies pixels. The floor is low enough to hold a
   whole grid of structures at once — that view is a map, not a reading. *)
let max_zoom = 2.0
let min_zoom = 0.035

let font ?(italic = false) ?(bold = false) size =
  let italic = match italic with true -> "italic " | false -> "" in
  let bold = match bold with true -> "700 " | false -> "" in
  sprintf
    "%s%s%.1fpx 'JetBrains Mono', ui-monospace, monospace"
    italic
    bold
    size
;;

(* wall-clock milliseconds, for pulse fades — [Js.date] because this jsoo's
   window object does not carry [performance] *)
let now () = Js.to_float (new%js Js.date_now)##getTime

let inject (state : State.t) action =
  Vdom.Effect.Expert.handle_non_dom_event_exn (state.input.inject action)
;;

(* ── styles ──────────────────────────────────────────────────────────── *)

let node_style (theme : Theme.t) (node : Heap_scene.Node.t) ~filter_active =
  let (fill, stroke, ink), dashed =
    match node.kind with
    | Heap_scene.Kind.Block -> theme.box_block, false
    | Heap_scene.Kind.Nil -> theme.box_nil, true
    | Heap_scene.Kind.Shared (_ : int) -> theme.box_shared, true
  in
  let dim = node.faded || (filter_active && not node.matched) in
  match dim with
  | false -> { Node_style.fill; stroke; ink; dashed }
  | true ->
    { Node_style.fill = Theme.fade theme fill
    ; stroke = Theme.fade theme stroke
    ; ink = Theme.fade theme ink
    ; dashed
    }
;;

let rebuild_styles (state : State.t) =
  let styles = String.Table.create () in
  List.iter state.input.roots ~f:(fun (root : Heap_scene.Root.t) ->
    Heap_scene.Node.fold root.node ~init:() ~f:(fun () node ->
      Hashtbl.set
        styles
        ~key:(Heap_layout.key_id node.key)
        ~data:
          (node_style
             state.input.theme
             node
             ~filter_active:state.input.filter_active)));
  state.styles <- styles
;;

(* ── geometry helpers ────────────────────────────────────────────────── *)

let layouts (state : State.t) = state.input.layouts

let box_of state id =
  Heap_layout.box_of (layouts state) ~tier_f:state.State.tier_f ~id
;;

let visible_placed (state : State.t) =
  (layouts state).(0).Heap_layout.Tier_layout.placed
;;

let world_of_screen (state : State.t) sx sy =
  (sx -. state.view.x) /. state.view.k, (sy -. state.view.y) /. state.view.k
;;

let node_at (state : State.t) sx sy =
  let wx, wy = world_of_screen state sx sy in
  let pad = 4. /. state.view.k in
  List.fold (visible_placed state) ~init:None ~f:(fun found placed ->
    match found with
    | Some (_ : Heap_layout.Placed.t) -> found
    | None ->
      (match box_of state placed.Heap_layout.Placed.id with
       | None -> None
       | Some box ->
         (match
            Float.( >= ) wx (box.x -. pad)
            && Float.( <= ) wx (box.x +. box.w +. pad)
            && Float.( >= ) wy (box.y -. pad)
            && Float.( <= ) wy (box.y +. box.h +. pad)
          with
          | true -> Some placed
          | false -> None)))
;;

let head_at (state : State.t) sx sy =
  let wx, wy = world_of_screen state sx sy in
  List.find
    (Heap_layout.heads_now (layouts state) ~tier_f:state.tier_f)
    ~f:(fun (head : Heap_layout.Head.t) ->
      Float.( >= ) wy head.y
      && Float.( <= ) wy (head.y +. 18.)
      && Float.( >= ) wx head.x
      && Float.( <= )
           wx
           (head.x
            +. (Float.of_int (String.length head.root.header + 12) *. 7.4)))
;;

let placed_by_id (state : State.t) id =
  List.find (visible_placed state) ~f:(fun placed ->
    String.equal placed.Heap_layout.Placed.id id)
;;

let set_anchor (state : State.t) sx sy =
  let wx, wy = world_of_screen state sx sy in
  let best =
    List.fold (visible_placed state) ~init:None ~f:(fun best placed ->
      match box_of state placed.Heap_layout.Placed.id with
      | None -> best
      | Some box ->
        let cx = box.x +. (box.w /. 2.) in
        let cy = box.y +. (box.h /. 2.) in
        let distance = Float.hypot (cx -. wx) (cy -. wy) in
        (match best with
         | Some ((_ : string), (_ : Heap_layout.Box.t), closest)
           when Float.( <= ) closest distance ->
           best
         | Some _ | None -> Some (placed.Heap_layout.Placed.id, box, distance)))
  in
  state.anchor
  <- (match best with
      | None -> Some (Anchor.Free { wx; wy; sx; sy })
      | Some (id, box, (_ : float)) ->
        (* clamp to the node's box so the anchor is always on real content —
           a point in the gap between nodes flies apart as the layout grows *)
        let nx = Float.clamp_exn ((wx -. box.x) /. box.w) ~min:0. ~max:1. in
        let ny = Float.clamp_exn ((wy -. box.y) /. box.h) ~min:0. ~max:1. in
        let px = box.x +. (nx *. box.w) in
        let py = box.y +. (ny *. box.h) in
        Some
          (Anchor.Node
             { id
             ; nx
             ; ny
             ; sx = (px *. state.view.k) +. state.view.x
             ; sy = (py *. state.view.k) +. state.view.y
             }))
;;

let apply_anchor (state : State.t) =
  match state.anchor with
  | None -> ()
  | Some (Anchor.Free { wx; wy; sx; sy }) ->
    state.view.x <- sx -. (wx *. state.view.k);
    state.view.y <- sy -. (wy *. state.view.k)
  | Some (Anchor.Node { id; nx; ny; sx; sy }) ->
    (match box_of state id with
     | None -> ()
     | Some box ->
       let wx = box.x +. (nx *. box.w) in
       let wy = box.y +. (ny *. box.h) in
       state.view.x <- sx -. (wx *. state.view.k);
       state.view.y <- sy -. (wy *. state.view.k))
;;

(* [min_tier] is the detail the fit refuses to go below: [0] normally reads
   the whole scene rather than shrinking it to nothing, so it stops where the
   labels do. Reflowing the grid passes a lower floor — there the point IS to
   see the new arrangement entire. *)
let fit ?(min_tier = 1.1) (state : State.t) =
  let bounds = (layouts state).(1) in
  match Float.( > ) bounds.width 0. && Float.( > ) state.width 0. with
  | false -> ()
  | true ->
    let pad = 90. in
    let kw = (state.width -. pad) /. bounds.width in
    let kh = (state.height -. pad) /. bounds.height in
    let k =
      Float.max
        (Float.max min_zoom (Heap_layout.k_for_tier min_tier))
        (Float.min (Heap_layout.k_for_tier 2.2) (Float.min kw kh))
    in
    state.view.k <- k;
    state.view.k_target <- k;
    state.tier_f <- Heap_layout.tier_for ~k;
    let width, height =
      Heap_layout.bounds_now (layouts state) ~tier_f:state.tier_f
    in
    state.view.x <- (state.width -. (width *. k)) /. 2.;
    state.view.y
    <- (match Float.( < ) (height *. k) (state.height -. 40.) with
        | true -> (state.height -. (height *. k)) /. 2.
        | false -> 26.);
    state.anchor <- None;
    state.dirty <- true
;;

(* the ids of the walked structure's own drawing — its root box and the
   subtree under it *)
let current_subtree_ids (state : State.t) =
  match state.input.current_structure_id with
  | None -> None
  | Some structure_id ->
    let rec find (node : Heap_scene.Node.t) =
      match
        node.key.structure_id = structure_id && List.is_empty node.key.path
      with
      | true -> Some node
      | false ->
        List.find_map node.children ~f:(fun ((_ : string), child) ->
          find child)
    in
    List.find_map state.input.roots ~f:(fun (root : Heap_scene.Root.t) ->
      find root.node)
    |> Option.map ~f:(fun node ->
      Heap_scene.Node.fold node ~init:[] ~f:(fun ids child ->
        Heap_layout.key_id child.key :: ids))
;;

(* the pinned box's own drawing — a single box, so focusing it is a close
   read of one node rather than a survey of a structure *)
let selected_ids (state : State.t) =
  match state.input.selected_address with
  | None -> None
  | Some selected ->
    (match
       List.filter_map (visible_placed state) ~f:(fun placed ->
         match placed.Heap_layout.Placed.node.address with
         | Some address when Snapshot.Address.equal address selected ->
           Some placed.id
         | Some (_ : Snapshot.Address.t) | None -> None)
     with
     | [] -> None
     | ids -> Some ids)
;;

(* [.] and [⌖]: the chosen marks filling the pane at reading detail — pan AND
   zoom, unconditionally *)
let focus_on_ids (state : State.t) ids =
  match ids with
  | [] -> ()
  | ids ->
    (* measured at the fields tier, which is where focusing lands *)
    let layout = (layouts state).(2) in
    let bounds =
      List.fold ids ~init:None ~f:(fun bounds id ->
        match Map.find layout.pos id with
        | None -> bounds
        | Some box ->
          (match bounds with
           | None -> Some (box.x, box.y, box.x +. box.w, box.y +. box.h)
           | Some (x0, y0, x1, y1) ->
             Some
               ( Float.min x0 box.x
               , Float.min y0 box.y
               , Float.max x1 (box.x +. box.w)
               , Float.max y1 (box.y +. box.h) )))
    in
    (match bounds with
     | None -> ()
     | Some (x0, y0, x1, y1) ->
       let bw = Float.max 1. (x1 -. x0) in
       let bh = Float.max 1. (y1 -. y0) in
       let k =
         Float.clamp_exn
           (Float.min
              ((state.width -. 90.) /. bw)
              ((state.height -. 120.) /. bh))
           ~min:(Heap_layout.k_for_tier 1.2)
           ~max:1.5
       in
       state.view.k <- k;
       state.view.k_target <- k;
       state.tier_f <- Heap_layout.tier_for ~k;
       (* the bbox was measured on the fields-tier layout; center on the same
          nodes at the tier actually landed on *)
       let cx, cy =
         let recentered =
           List.fold ids ~init:None ~f:(fun bounds id ->
             match box_of state id with
             | None -> bounds
             | Some box ->
               (match bounds with
                | None -> Some (box.x, box.y, box.x +. box.w, box.y +. box.h)
                | Some (x0, y0, x1, y1) ->
                  Some
                    ( Float.min x0 box.x
                    , Float.min y0 box.y
                    , Float.max x1 (box.x +. box.w)
                    , Float.max y1 (box.y +. box.h) )))
         in
         match recentered with
         | None -> (x0 +. x1) /. 2., (y0 +. y1) /. 2.
         | Some (x0, y0, x1, y1) -> (x0 +. x1) /. 2., (y0 +. y1) /. 2.
       in
       state.view.x <- (state.width /. 2.) -. (cx *. k);
       state.view.y <- (state.height /. 2.) -. (cy *. k);
       state.anchor <- None;
       state.dirty <- true)
;;

(* The two marks the pane carries, and what [⌖] alternates between: the blue
   box you pinned, and the orange structure this step walked. Asking for the
   selection when nothing is pinned falls through to the walked one rather
   than doing nothing — an inert button reads as broken. *)
let focus_on_target (state : State.t) =
  let current () =
    focus_on_ids state (Option.value (current_subtree_ids state) ~default:[])
  in
  match state.input.focus_target with
  | Action.Focus_target.Current -> current ()
  | Action.Focus_target.Selection ->
    (match selected_ids state with
     | Some ids -> focus_on_ids state ids
     | None -> current ())
;;

(* stepping lands the eye on the walked structure — pan, never rezoom, and
   only when it is off screen *)
let land_on_current (state : State.t) =
  match state.input.current_root_id with
  | None -> ()
  | Some id ->
    (match box_of state id with
     | None -> ()
     | Some box ->
       let sx = (box.x *. state.view.k) +. state.view.x in
       let sy = (box.y *. state.view.k) +. state.view.y in
       let sw = box.w *. state.view.k in
       let sh = box.h *. state.view.k in
       let visible =
         Float.( >= ) (sx +. sw) 40.
         && Float.( <= ) sx (state.width -. 40.)
         && Float.( >= ) (sy +. sh) 40.
         && Float.( <= ) sy (state.height -. 60.)
       in
       (match visible with
        | true -> ()
        | false ->
          state.view.x
          <- (state.width /. 2.) -. ((box.x +. (box.w /. 2.)) *. state.view.k);
          state.view.y <- (state.height /. 3.) -. (box.y *. state.view.k);
          state.anchor <- None;
          state.dirty <- true))
;;

(* ── drawing ─────────────────────────────────────────────────────────── *)

let set_fill (context : Dom_html.canvasRenderingContext2D Js.t) color =
  context##.fillStyle := Js.string color
;;

let set_stroke (context : Dom_html.canvasRenderingContext2D Js.t) color =
  context##.strokeStyle := Js.string color
;;

let set_dash (context : Dom_html.canvasRenderingContext2D Js.t) segments
  : unit
  =
  Js.Unsafe.meth_call
    context
    "setLineDash"
    [| Js.Unsafe.inject (Js.array (Array.of_list segments)) |]
;;

let fit_text label ~size ~room =
  let capacity = Int.of_float (room /. (size *. 0.6)) in
  match String.length label > capacity with
  | false -> label
  | true ->
    let keep = Int.max 1 (capacity - 1) in
    String.prefix label keep ^ "…"
;;

let line_parts
  (theme : Theme.t)
  (line : Heap_scene.Line.t)
  ~(style : Node_style.t)
  =
  List.map line ~f:(fun (part : Heap_scene.Line.Part.t) ->
    match part with
    | Key text -> text, style.ink, false
    | Value text -> text, theme.node_value, false
    | Label text -> text, theme.node_key, false
    | Arrow -> " → ", theme.node_key, false
    | Null -> "null", theme.edge_label, true)
;;

let draw_node
  (state : State.t)
  (placed : Heap_layout.Placed.t)
  (box : Heap_layout.Box.t)
  ~tier
  ~raised
  =
  let context = state.context in
  let theme = state.input.theme in
  let node = placed.node in
  let style =
    Option.value
      (Hashtbl.find state.styles placed.id)
      ~default:
        { Node_style.fill = theme.bg
        ; stroke = theme.edge
        ; ink = theme.text
        ; dashed = false
        }
  in
  let k = state.view.k in
  let hovered =
    Option.value_map state.hover ~default:false ~f:(String.equal placed.id)
  in
  let selected =
    match state.input.selected_address, node.address with
    | Some selected, Some address -> Snapshot.Address.equal selected address
    | (Some _ | None), (Some _ | None) -> false
  in
  let ct = Heap_layout.content_tier tier in
  let box =
    match raised with
    | false -> box
    | true ->
      (* focal mode: the box near the cursor grows to its own tier's size and
         floats over its neighbours *)
      let split = Heap_layout.split tier in
      let wa, ha = Heap_layout.size node ~tier:split.a in
      let wb, hb = Heap_layout.size node ~tier:split.b in
      let w = wa +. ((wb -. wa) *. split.f) in
      let h = ha +. ((hb -. ha) *. split.f) in
      { Heap_layout.Box.x = box.x +. (box.w /. 2.) -. (w /. 2.)
      ; y = box.y +. (box.h /. 2.) -. (h /. 2.)
      ; w
      ; h
      }
  in
  (match raised with
   | false -> ()
   | true ->
     context##save;
     context##.shadowColor := Js.string "rgba(0,0,0,.75)";
     context##.shadowBlur := Js.float (22. /. k);
     context##.shadowOffsetY := Js.float (5. /. k));
  (* Zoomed out there is no text and no border, so the box itself has to be
     the ink: below the first tier the fill lifts to the border color — what
     the minimap draws with — and settles back onto the panel fill as the
     labels arrive. A panel fill is a surface; at postage-stamp size a
     surface is invisible. *)
  set_fill
    context
    (match Float.( < ) tier 1. with
     | false -> style.fill
     | true -> Theme.mix style.fill style.stroke ~amount:(1. -. tier));
  context##fillRect
    (Js.float box.x)
    (Js.float box.y)
    (Js.float box.w)
    (Js.float box.h);
  (match raised with
   | false -> ()
   | true ->
     context##.shadowColor := Js.string "transparent";
     context##.shadowBlur := Js.float 0.;
     context##.shadowOffsetY := Js.float 0.);
  let border_visible = ct >= 1 || hovered || selected in
  (match border_visible with
   | false -> ()
   | true ->
     let color =
       match hovered, selected with
       | true, (_ : bool) -> theme.accent_bright
       | false, true -> theme.selection_border
       | false, false -> style.stroke
     in
     (match node.is_new && not (hovered || selected) with
      | false -> set_stroke context color
      | true -> set_stroke context theme.fresh);
     context##.lineWidth
     := Js.float
          ((match hovered || selected with true -> 1.4 | false -> 1.) /. k);
     (match style.dashed with
      | true -> set_dash context [ Js.float (3. /. k); Js.float (3. /. k) ]
      | false -> ());
     context##strokeRect
       (Js.float (box.x +. (0.5 /. k)))
       (Js.float (box.y +. (0.5 /. k)))
       (Js.float (box.w -. (1. /. k)))
       (Js.float (box.h -. (1. /. k)));
     (match style.dashed with true -> set_dash context [] | false -> ()));
  (* fold marker: the box stays, the subtree is behind it *)
  (match node.folded && ct >= 1 with
   | false -> ()
   | true ->
     set_fill context theme.accent;
     context##.font := Js.string (font 10.);
     context##.textAlign := Js.string "right";
     context##.textBaseline := Js.string "top";
     context##fillText
       (Js.string [%string "▸%{node.hidden_count#Int}"])
       (Js.float (box.x +. box.w -. 4.))
       (Js.float (box.y +. 3.));
     context##.textAlign := Js.string "left");
  (match node.is_new && ct >= 2 with
   | false -> ()
   | true ->
     set_fill context theme.fresh;
     context##.font := Js.string (font 9.5);
     context##.textAlign := Js.string "right";
     context##.textBaseline := Js.string "top";
     context##fillText
       (Js.string "new")
       (Js.float (box.x +. box.w -. 5.))
       (Js.float (box.y +. 4.));
     context##.textAlign := Js.string "left");
  (* clipped content *)
  context##save;
  context##beginPath;
  context##rect
    (Js.float box.x)
    (Js.float box.y)
    (Js.float box.w)
    (Js.float box.h);
  context##clip;
  context##.textBaseline := Js.string "top";
  (match node.kind, ct with
   | Heap_scene.Kind.Nil, ct when ct >= 1 ->
     (* the null mark is drawn, not typed: the [∅] glyph is at the mercy of
        whichever font the canvas fell back to *)
     let cx = box.x +. (box.w /. 2.) in
     let cy = box.y +. (box.h /. 2.) in
     let radius = Float.min box.w box.h *. 0.22 in
     set_stroke context style.ink;
     context##.lineWidth := Js.float (1.2 /. k);
     context##beginPath;
     context##arc
       (Js.float cx)
       (Js.float cy)
       (Js.float radius)
       (Js.float 0.)
       (Js.float (2. *. Float.pi))
       Js._false;
     context##stroke;
     let reach = radius *. 1.45 in
     context##beginPath;
     context##moveTo (Js.float (cx -. reach)) (Js.float (cy +. reach));
     context##lineTo (Js.float (cx +. reach)) (Js.float (cy -. reach));
     context##stroke
   | Heap_scene.Kind.Nil, (_ : int) -> ()
   | (Heap_scene.Kind.Block | Heap_scene.Kind.Shared _), 1 ->
     set_fill context style.ink;
     context##.font := Js.string (font ~bold:true 17.);
     context##.textAlign := Js.string "center";
     context##fillText
       (Js.string (fit_text node.label ~size:17. ~room:(box.w -. 14.)))
       (Js.float (box.x +. (box.w /. 2.)))
       (Js.float (box.y +. ((box.h -. 17.) /. 2.) -. 1.));
     context##.textAlign := Js.string "left"
   | (Heap_scene.Kind.Block | Heap_scene.Kind.Shared _), (2 | 3) ->
     (* the node's own name reads as a header: white, bold, over a rule *)
     set_fill context style.ink;
     context##.font := Js.string (font ~bold:true 16.);
     context##fillText
       (Js.string (fit_text node.label ~size:16. ~room:(box.w -. 20.)))
       (Js.float (box.x +. 10.))
       (Js.float (box.y +. 7.));
     set_stroke context (Theme.mix style.stroke theme.bg ~amount:0.45);
     context##.lineWidth := Js.float (1. /. k);
     context##beginPath;
     context##moveTo (Js.float box.x) (Js.float (box.y +. 31.));
     context##lineTo (Js.float (box.x +. box.w)) (Js.float (box.y +. 31.));
     context##stroke;
     let y = ref (box.y +. 37.) in
     List.iter node.lines ~f:(fun line ->
       let x = ref (box.x +. 10.) in
       List.iter
         (line_parts theme line ~style)
         ~f:(fun (text, color, italic) ->
           context##.font := Js.string (font ~italic 15.);
           set_fill context color;
           let room = box.x +. box.w -. 10. -. !x in
           match Float.( > ) room 8. with
           | false -> ()
           | true ->
             let fitted = fit_text text ~size:15. ~room in
             context##fillText (Js.string fitted) (Js.float !x) (Js.float !y);
             x := !x +. (Float.of_int (String.length fitted) *. 9.));
       y := !y +. 22.);
     (match ct = 3 && not (List.is_empty node.raw) with
      | false -> ()
      | true ->
        let y0 = !y +. 4. in
        set_stroke context (Theme.mix style.stroke theme.bg ~amount:0.6);
        set_dash context [ Js.float (2. /. k); Js.float (2. /. k) ];
        context##beginPath;
        context##moveTo (Js.float (box.x +. 6.)) (Js.float y0);
        context##lineTo (Js.float (box.x +. box.w -. 6.)) (Js.float y0);
        context##stroke;
        set_dash context [];
        context##.font := Js.string (font 13.);
        let ry = ref (y0 +. 7.) in
        List.iter node.raw ~f:(fun (key, value) ->
          set_fill context theme.raw_key;
          context##fillText
            (Js.string key)
            (Js.float (box.x +. 10.))
            (Js.float !ry);
          set_fill context theme.raw_value;
          context##fillText
            (Js.string (fit_text value ~size:13. ~room:(box.w -. 62.)))
            (Js.float (box.x +. 10. +. 36.))
            (Js.float !ry);
          ry := !ry +. 18.))
   | (Heap_scene.Kind.Block | Heap_scene.Kind.Shared _), (_ : int) -> ());
  context##restore;
  match raised with true -> context##restore | false -> ()
;;

let draw_edges (state : State.t) ~content_alpha =
  let context = state.context in
  let theme = state.input.theme in
  let k = state.view.k in
  context##.lineWidth := Js.float (Float.max (0.6 /. k) (1. /. k));
  List.iter (visible_placed state) ~f:(fun placed ->
    match placed.Heap_layout.Placed.parent with
    | None -> ()
    | Some parent_id ->
      (match box_of state parent_id, box_of state placed.id with
       | None, (Some _ | None) | Some _, None -> ()
       | Some parent, Some child ->
         let x1 = parent.x +. (parent.w /. 2.) in
         let y1 = parent.y +. parent.h in
         let x2 = child.x +. (child.w /. 2.) in
         let y2 = child.y in
         set_stroke
           context
           (match state.input.filter_active with
            | true -> theme.hairline
            | false -> theme.edge);
         context##beginPath;
         (match state.input.edge_style with
          | Action.Edge_style.Orthogonal ->
            let my = (y1 +. y2) /. 2. in
            context##moveTo (Js.float x1) (Js.float y1);
            context##lineTo (Js.float x1) (Js.float my);
            context##lineTo (Js.float x2) (Js.float my);
            context##lineTo (Js.float x2) (Js.float y2)
          | Action.Edge_style.Curved ->
            let dy = Float.max 12. ((y2 -. y1) *. 0.5) in
            context##moveTo (Js.float x1) (Js.float y1);
            context##bezierCurveTo
              (Js.float x1)
              (Js.float (y1 +. dy))
              (Js.float x2)
              (Js.float (y2 -. dy))
              (Js.float x2)
              (Js.float y2)
          | Action.Edge_style.Angled ->
            let span = y2 -. y1 in
            let dip = Float.min 10. (span *. 0.22) in
            context##moveTo (Js.float x1) (Js.float y1);
            context##lineTo (Js.float x1) (Js.float (y1 +. dip));
            context##lineTo (Js.float x2) (Js.float (y2 -. dip));
            context##lineTo (Js.float x2) (Js.float y2));
         context##stroke;
         (match
            Float.( > ) content_alpha 0.04
            && Float.( > ) k 0.3
            && not (String.is_empty placed.edge_label)
          with
          | false -> ()
          | true ->
            context##.font := Js.string (font 10.5);
            context##.textAlign := Js.string "center";
            context##.textBaseline := Js.string "middle";
            let mx = (x1 +. x2) /. 2. in
            let my = (y1 +. y2) /. 2. in
            let width =
              Float.of_int (String.length placed.edge_label) *. 6.3
            in
            set_fill context theme.bg;
            context##fillRect
              (Js.float (mx -. (width /. 2.) -. 3.))
              (Js.float (my -. 7.))
              (Js.float (width +. 6.))
              (Js.float 14.);
            set_fill context theme.edge_label;
            context##fillText
              (Js.string placed.edge_label)
              (Js.float mx)
              (Js.float my);
            context##.textAlign := Js.string "left";
            context##.textBaseline := Js.string "top")))
;;

let draw_heads (state : State.t) =
  let context = state.context in
  let theme = state.input.theme in
  match Heap_layout.content_tier state.tier_f >= 1 with
  | false -> ()
  | true ->
    context##.font := Js.string (font ~bold:true 15.);
    context##.textBaseline := Js.string "alphabetic";
    List.iter
      (Heap_layout.heads_now (layouts state) ~tier_f:state.tier_f)
      ~f:(fun (head : Heap_layout.Head.t) ->
        let root = head.root in
        let name, rest =
          match String.lsplit2 root.header ~on:'\183' with
          (* '·' is multi-byte; split on the raw string instead *)
          | Some ((_ : string), (_ : string)) | None ->
            (match String.substr_index root.header ~pattern:" · " with
             | None -> root.header, ""
             | Some at ->
               ( String.prefix root.header at
               , String.drop_prefix root.header at ))
        in
        let accent, dim =
          match root.faded with
          | false -> theme.accent, theme.dim
          | true -> Theme.fade theme theme.accent, Theme.fade theme theme.dim
        in
        let glyph =
          match root.node.folded with true -> "▸ " | false -> "▾ "
        in
        set_fill context accent;
        context##fillText
          (Js.string [%string "%{glyph}%{name}"])
          (Js.float head.x)
          (Js.float (head.y +. 14.));
        let prefix_width =
          Js.to_float
            (context##measureText (Js.string [%string "%{glyph}%{name} "]))##.
            width
        in
        let nodes_word =
          match root.count with 1 -> "node" | (_ : int) -> "nodes"
        in
        set_fill context dim;
        context##fillText
          (Js.string [%string "%{rest} · %{root.count#Int} %{nodes_word}"])
          (Js.float (head.x +. prefix_width))
          (Js.float (head.y +. 14.)))
;;

let draw_mini (state : State.t) =
  let context = state.context in
  let theme = state.input.theme in
  let mw = 168. in
  let mh = 118. in
  let pad = 14. in
  let x = state.width -. mw -. pad in
  let y = state.height -. mh -. pad in
  let tier =
    Int.max 0 (Int.min 3 (Int.of_float (Float.round_nearest state.tier_f)))
  in
  let layout = (layouts state).(tier) in
  (* the map is scaled to the layout it actually DRAWS, not to the crossfaded
     bounds: those belong to a size between two tiers, and a tier whose boxes
     are bigger than that spilled out of the frame *)
  let scale =
    Float.min
      ((mw -. 12.) /. Float.max 1. layout.width)
      ((mh -. 12.) /. Float.max 1. layout.height)
  in
  state.mini <- Some { Mini.x; y; w = mw; h = mh; scale };
  context##save;
  set_fill context theme.minimap_bg;
  context##fillRect (Js.float x) (Js.float y) (Js.float mw) (Js.float mh);
  set_stroke context theme.panel_border;
  context##.lineWidth := Js.float 1.;
  context##strokeRect
    (Js.float (x +. 0.5))
    (Js.float (y +. 0.5))
    (Js.float (mw -. 1.))
    (Js.float (mh -. 1.));
  (* and it is clipped as well: the 3px floor under a box's drawn size keeps
     a postage-stamp node visible, and near the edge that floor is what
     reaches past the frame *)
  context##beginPath;
  context##rect
    (Js.float (x +. 1.))
    (Js.float (y +. 1.))
    (Js.float (mw -. 2.))
    (Js.float (mh -. 2.));
  context##clip;
  context##translate (Js.float (x +. 6.)) (Js.float (y +. 6.));
  context##scale (Js.float scale) (Js.float scale);
  List.iter layout.placed ~f:(fun placed ->
    match Map.find layout.pos placed.Heap_layout.Placed.id with
    | None -> ()
    | Some box ->
      set_fill
        context
        (match placed.node.kind with
         | Heap_scene.Kind.Shared (_ : int) -> theme.minimap_shared
         | Heap_scene.Kind.Nil -> theme.minimap_nil
         | Heap_scene.Kind.Block -> theme.minimap_block);
      context##fillRect
        (Js.float box.x)
        (Js.float box.y)
        (Js.float (Float.max box.w (3. /. scale)))
        (Js.float (Float.max box.h (3. /. scale))));
  context##restore;
  (* the viewport frame *)
  let vx = -.state.view.x /. state.view.k in
  let vy = -.state.view.y /. state.view.k in
  context##save;
  context##beginPath;
  context##rect
    (Js.float (x +. 1.))
    (Js.float (y +. 1.))
    (Js.float (mw -. 2.))
    (Js.float (mh -. 2.));
  context##clip;
  set_stroke context theme.gold;
  context##.lineWidth := Js.float 1.;
  context##strokeRect
    (Js.float (x +. 6. +. (vx *. scale)))
    (Js.float (y +. 6. +. (vy *. scale)))
    (Js.float (state.width /. state.view.k *. scale))
    (Js.float (state.height /. state.view.k *. scale));
  context##restore
;;

let draw_tooltip (state : State.t) =
  match state.hover, state.cursor with
  | None, (_ : (float * float) option) | (_ : string option), None -> ()
  | Some id, Some (sx, sy) ->
    (match placed_by_id state id with
     | None -> ()
     | Some placed ->
       let node = placed.node in
       let context = state.context in
       let theme = state.input.theme in
       let address =
         match node.address with
         | Some address -> Snapshot.Address.display address
         | None -> "—"
       in
       let header =
         [%string
           "%{address}  ·  %{node.words#Int}w  ·  %{node.words * 8#Int}B"]
       in
       let lines =
         [ node.label, theme.bright; header, theme.dim ]
         @ List.map (List.take node.lines 8) ~f:(fun line ->
           "  " ^ Heap_scene.Line.text line, theme.text)
         @ (match List.length node.lines > 8 with
            | true ->
              [ ( [%string "  … %{List.length node.lines - 8#Int} more"]
                , theme.faint )
              ]
            | false -> [])
         @ (match node.folded with
            | true ->
              [ ( [%string "  folded — h unfolds ⋯ %{node.hidden_count#Int}"]
                , theme.accent )
              ]
            | false -> [])
         @
         match node.faded with
         | true -> [ "  name no longer reaches this", theme.faint ]
         | false -> []
       in
       let width =
         List.fold lines ~init:120. ~f:(fun widest (text, (_ : string)) ->
           Float.max
             widest
             ((Float.of_int (String.length text) *. 6.6) +. 20.))
         |> Float.min 360.
       in
       let height = 14. +. (Float.of_int (List.length lines) *. 16.) in
       let x = Float.min (sx +. 18.) (state.width -. width -. 8.) in
       let y = Float.min (sy +. 16.) (state.height -. height -. 8.) in
       set_fill context theme.tooltip_bg;
       context##fillRect
         (Js.float x)
         (Js.float y)
         (Js.float width)
         (Js.float height);
       set_stroke context theme.tooltip_border;
       context##.lineWidth := Js.float 1.;
       context##strokeRect
         (Js.float (x +. 0.5))
         (Js.float (y +. 0.5))
         (Js.float (width -. 1.))
         (Js.float (height -. 1.));
       context##.textBaseline := Js.string "top";
       context##.font := Js.string (font 11.5);
       List.iteri lines ~f:(fun index (text, color) ->
         set_fill context color;
         context##fillText
           (Js.string (fit_text text ~size:11.5 ~room:(width -. 16.)))
           (Js.float (x +. 8.))
           (Js.float (y +. 7. +. (Float.of_int index *. 16.)))))
;;

let sync_hud (state : State.t) =
  let zoom_percent =
    Int.of_float (Float.round_nearest (state.view.k *. 100.))
  in
  let tier = Float.round_decimal state.tier_f ~decimal_digits:1 in
  match zoom_percent = state.hud_zoom && Float.equal tier state.hud_tier with
  | true -> ()
  | false ->
    state.hud_zoom <- zoom_percent;
    state.hud_tier <- tier;
    inject state (Action.Set_hud { zoom_percent; tier })
;;

let draw (state : State.t) =
  let context = state.context in
  let theme = state.input.theme in
  context##setTransform
    (Js.float state.dpr)
    (Js.float 0.)
    (Js.float 0.)
    (Js.float state.dpr)
    (Js.float 0.)
    (Js.float 0.);
  set_fill context theme.bg;
  context##fillRect
    (Js.float 0.)
    (Js.float 0.)
    (Js.float state.width)
    (Js.float state.height);
  context##save;
  context##translate (Js.float state.view.x) (Js.float state.view.y);
  context##scale (Js.float state.view.k) (Js.float state.view.k);
  let content_alpha =
    match Heap_layout.content_tier state.tier_f >= 1 with
    | true -> 1.
    | false -> 0.
  in
  draw_heads state;
  draw_edges state ~content_alpha;
  let focal =
    match state.input.lod with
    | Action.Lod.Focal -> true
    | Action.Lod.Uniform -> false
  in
  let global_tier =
    match focal with
    | true -> Float.min state.tier_f 1.45
    | false -> state.tier_f
  in
  let raised = ref [] in
  List.iter (visible_placed state) ~f:(fun placed ->
    match box_of state placed.Heap_layout.Placed.id with
    | None -> ()
    | Some box ->
      let sx = (box.x *. state.view.k) +. state.view.x in
      let sy = (box.y *. state.view.k) +. state.view.y in
      let sw = box.w *. state.view.k in
      let sh = box.h *. state.view.k in
      (match
         Float.( > ) sx (state.width +. 80.)
         || Float.( < ) (sx +. sw) (-80.)
         || Float.( > ) sy (state.height +. 80.)
         || Float.( < ) (sy +. sh) (-80.)
       with
       | true -> ()
       | false ->
         let tier =
           match focal, state.cursor with
           | false, (_ : (float * float) option) | true, None -> global_tier
           | true, Some (cx, cy) ->
             let bx = sx +. (sw /. 2.) in
             let by = sy +. (sh /. 2.) in
             let distance = Float.hypot (bx -. cx) (by -. cy) in
             Float.min
               3.
               (global_tier
                +. ((1.55 +. (state.tier_f *. 0.5))
                    *. Float.exp (-.(distance /. 190.) *. (distance /. 190.))
                   ))
         in
         (match focal && Float.( > ) tier (global_tier +. 0.3) with
          | true -> raised := (placed, box, tier) :: !raised
          | false -> draw_node state placed box ~tier ~raised:false)));
  List.iter
    (List.sort
       !raised
       ~compare:
         (fun
           ((_ : Heap_layout.Placed.t), (_ : Heap_layout.Box.t), a)
           ((_ : Heap_layout.Placed.t), (_ : Heap_layout.Box.t), b)
         -> Float.compare a b))
    ~f:(fun (placed, box, tier) ->
      draw_node state placed box ~tier ~raised:true);
  (* pulses: rings widening off boxes this step allocated *)
  let time = now () in
  state.pulses
  <- List.filter state.pulses ~f:(fun pulse ->
       Float.( < ) (time -. pulse.started) 1100.);
  List.iter state.pulses ~f:(fun pulse ->
    match box_of state pulse.id with
    | None -> ()
    | Some box ->
      let age = (time -. pulse.started) /. 1100. in
      context##.globalAlpha := Js.float (Float.max 0. (1. -. age) *. 0.9);
      set_stroke context theme.gold;
      context##.lineWidth := Js.float (1.5 /. state.view.k);
      let growth = age *. 10. in
      context##strokeRect
        (Js.float (box.x -. growth))
        (Js.float (box.y -. growth))
        (Js.float (box.w +. (growth *. 2.)))
        (Js.float (box.h +. (growth *. 2.)));
      context##.globalAlpha := Js.float 1.);
  context##restore;
  draw_mini state;
  draw_tooltip state;
  sync_hud state
;;

(* ── interaction ─────────────────────────────────────────────────────── *)

let resize (state : State.t) =
  let rect = state.canvas##getBoundingClientRect in
  let width = Js.to_float rect##.right -. Js.to_float rect##.left in
  let height = Js.to_float rect##.bottom -. Js.to_float rect##.top in
  state.dpr <- Float.min 2. (Js.to_float Dom_html.window##.devicePixelRatio);
  state.width <- width;
  state.height <- height;
  state.canvas##.width := Int.max 1 (Int.of_float (width *. state.dpr));
  state.canvas##.height := Int.max 1 (Int.of_float (height *. state.dpr));
  state.dirty <- true;
  match state.fitted with
  | true -> ()
  | false ->
    (match Float.( > ) width 0. with
     | false -> ()
     | true ->
       state.fitted <- true;
       fit state)
;;

let canvas_position (state : State.t) (event : #Dom_html.mouseEvent Js.t) =
  let rect = state.canvas##getBoundingClientRect in
  ( Js.to_float event##.clientX -. Js.to_float rect##.left
  , Js.to_float event##.clientY -. Js.to_float rect##.top )
;;

let on_wheel (state : State.t) (event : Dom_html.wheelEvent Js.t) =
  Dom.preventDefault event;
  let sx, sy = canvas_position state event in
  set_anchor state sx sy;
  let delta = Js.to_float event##.deltaY in
  let ctrl = Js.to_bool event##.ctrlKey in
  let factor =
    Float.exp (-.delta *. match ctrl with true -> 0.012 | false -> 0.0022)
  in
  state.view.k_target
  <- Float.clamp_exn
       (state.view.k_target *. factor)
       ~min:min_zoom
       ~max:max_zoom
;;

let mini_to (state : State.t) sx sy =
  match state.mini with
  | None -> ()
  | Some mini ->
    let wx = (sx -. mini.x -. 6.) /. mini.scale in
    let wy = (sy -. mini.y -. 6.) /. mini.scale in
    state.view.x <- (state.width /. 2.) -. (wx *. state.view.k);
    state.view.y <- (state.height /. 2.) -. (wy *. state.view.k);
    state.anchor <- None;
    state.dirty <- true
;;

let in_mini (state : State.t) sx sy =
  match state.mini with
  | None -> false
  | Some mini ->
    Float.( > ) sx mini.x
    && Float.( > ) sy mini.y
    && Float.( < ) sx (mini.x +. mini.w)
    && Float.( < ) sy (mini.y +. mini.h)
;;

let on_pointer_down (state : State.t) (event : Dom_html.mouseEvent Js.t) =
  let sx, sy = canvas_position state event in
  match in_mini state sx sy with
  | true ->
    state.mini_drag <- true;
    mini_to state sx sy
  | false ->
    state.drag
    <- Some
         ( Js.to_float event##.clientX
         , Js.to_float event##.clientY
         , state.view.x
         , state.view.y );
    state.drag_moved <- false;
    state.canvas##.style##.cursor := Js.string "grabbing"
;;

let on_pointer_move (state : State.t) (event : Dom_html.mouseEvent Js.t) =
  let sx, sy = canvas_position state event in
  state.cursor <- Some (sx, sy);
  match state.mini_drag, state.drag with
  | true, (_ : (float * float * float * float) option) -> mini_to state sx sy
  | false, Some (px, py, vx, vy) ->
    let dx = Js.to_float event##.clientX -. px in
    let dy = Js.to_float event##.clientY -. py in
    (match Float.( > ) (Float.hypot dx dy) 3. with
     | true -> state.drag_moved <- true
     | false -> ());
    state.view.x <- vx +. dx;
    state.view.y <- vy +. dy;
    state.anchor <- None;
    state.dirty <- true
  | false, None ->
    let hover =
      Option.map (node_at state sx sy) ~f:(fun placed ->
        placed.Heap_layout.Placed.id)
    in
    (match state.input.lod with
     | Action.Lod.Focal -> state.dirty <- true
     | Action.Lod.Uniform -> ());
    (match [%equal: string option] hover state.hover with
     | true ->
       (* tooltip follows the pointer while over one box *)
       (match Option.is_some hover with
        | true -> state.dirty <- true
        | false -> ())
     | false ->
       state.hover <- hover;
       state.dirty <- true)
;;

let on_pointer_up (state : State.t) (event : Dom_html.mouseEvent Js.t) =
  let was_drag = state.drag_moved in
  let dragging = Option.is_some state.drag in
  state.mini_drag <- false;
  state.drag <- None;
  state.drag_moved <- false;
  state.canvas##.style##.cursor := Js.string "grab";
  match dragging && not was_drag with
  | false -> ()
  | true ->
    let sx, sy = canvas_position state event in
    (match in_mini state sx sy with
     | true -> ()
     | false ->
       (match node_at state sx sy with
        | Some placed ->
          (* committing a box is the TUI's click: pin it blue and jump the
             replay to where it was allocated *)
          (match placed.node.address with
           | Some address ->
             inject state (Action.Select_heap_address address)
           | None -> ())
        | None ->
          (match head_at state sx sy with
           | Some head ->
             inject
               state
               (Action.Toggle_heap_fold
                  (Heap_scene.Fold_key.root head.root.structure_id))
           | None -> ())))
;;

let on_pointer_leave (state : State.t) (_ : Dom_html.mouseEvent Js.t) =
  state.hover <- None;
  state.cursor <- None;
  state.dirty <- true
;;

let on_double_click (state : State.t) (event : Dom_html.mouseEvent Js.t) =
  let sx, sy = canvas_position state event in
  match node_at state sx sy with
  | None -> fit state
  | Some (_ : Heap_layout.Placed.t) ->
    set_anchor state sx sy;
    state.view.k_target <- Float.min max_zoom (state.view.k_target *. 2.3)
;;

let zoom_by (state : State.t) factor =
  set_anchor state (state.width /. 2.) (state.height /. 2.);
  state.view.k_target
  <- Float.clamp_exn
       (state.view.k_target *. factor)
       ~min:min_zoom
       ~max:max_zoom
;;

let pan_by (state : State.t) dx dy =
  state.view.x <- state.view.x +. dx;
  state.view.y <- state.view.y +. dy;
  state.anchor <- None;
  state.dirty <- true
;;

let hovered_fold_key (state : State.t) =
  match state.hover with
  | Some id ->
    Option.map (placed_by_id state id) ~f:(fun placed -> placed.node.key)
  | None ->
    (* nothing aimed at: fold the structure the step walked, the TUI's
       fallback *)
    Option.map state.input.current_structure_id ~f:Heap_scene.Fold_key.root
;;

let on_key (state : State.t) (event : Dom_html.keyboardEvent Js.t) =
  let target_is_input =
    Js.Opt.case
      event##.target
      (fun () -> false)
      (fun target ->
        Js.Opt.case
          (Dom_html.CoerceTo.element target)
          (fun () -> false)
          (fun element ->
            String.equal
              (String.uppercase (Js.to_string element##.tagName))
              "INPUT"))
  in
  match target_is_input with
  | true -> ()
  | false ->
    let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
    let act ?(prevent = true) action =
      (match prevent with true -> Dom.preventDefault event | false -> ());
      inject state action
    in
    (match key with
     | "ArrowRight" | "l" | "n" -> act (Action.Step_delta 1)
     | "ArrowLeft" | "p" -> act (Action.Step_delta (-1))
     | "ArrowUp" -> act (Action.Frame_delta (-1))
     | "ArrowDown" -> act (Action.Frame_delta 1)
     | " " -> act Action.Toggle_play
     | "g" | "Home" -> act (Action.Step_to 0)
     | "G" | "End" -> act (Action.Step_to Int.max_value)
     | "h" ->
       (match hovered_fold_key state with
        | Some fold -> act (Action.Toggle_heap_fold fold)
        | None -> ())
     | "v" -> act Action.Toggle_heap_view
     | "z" -> act Action.Toggle_accordion
     | "Z" -> act Action.Reset_flame_zoom
     | "m" -> act Action.Toggle_lod
     | "e" -> act Action.Cycle_edge_style
     | "t" -> act Action.Toggle_theme
     | "o" -> act Action.Toggle_address_order
     | "." -> act Action.Focus_latest
     | "/" -> act Action.Begin_filter
     | "Escape" -> act Action.Cancel_filter
     | "f" -> act Action.Toggle_flame
     | "1" -> act Action.Toggle_stack_pane
     | "2" -> act Action.Toggle_source_pane
     | "q" -> act Action.Quit
     | "0" ->
       Dom.preventDefault event;
       fit state
     | "=" | "+" ->
       Dom.preventDefault event;
       zoom_by state 1.6
     | "-" ->
       Dom.preventDefault event;
       zoom_by state (1. /. 1.6)
     | "w" -> pan_by state 0. 60.
     | "s" -> pan_by state 0. (-60.)
     | "a" -> pan_by state 60. 0.
     | "d" -> pan_by state (-60.) 0.
     | "PageUp" ->
       Dom.preventDefault event;
       pan_by state 0. (state.height *. 0.8)
     | "PageDown" ->
       Dom.preventDefault event;
       pan_by state 0. (-.(state.height *. 0.8))
     | (_ : string) -> ())
;;

(* ── lifecycle ───────────────────────────────────────────────────────── *)

let rec loop (state : State.t) (_ : float) =
  let k = state.view.k in
  let k_target = state.view.k_target in
  (match Float.( > ) (Float.abs (k_target -. k)) 1e-4 with
   | true ->
     state.view.k <- k +. ((k_target -. k) *. 0.24);
     state.tier_f <- Heap_layout.tier_for ~k:state.view.k;
     apply_anchor state;
     state.dirty <- true
   | false ->
     (match Float.equal k k_target with
      | true -> ()
      | false ->
        state.view.k <- k_target;
        state.tier_f <- Heap_layout.tier_for ~k:k_target;
        apply_anchor state;
        state.dirty <- true));
  (match List.is_empty state.pulses with
   | true -> ()
   | false -> state.dirty <- true);
  (match state.dirty with
   | false -> ()
   | true ->
     draw state;
     state.dirty <- false);
  state.raf
  <- Some
       (Dom_html.window##requestAnimationFrame
          (Js.wrap_callback (fun time -> loop state (Js.to_float time))))
;;

let listen target event handler =
  ignore
    (Dom.addEventListener
       target
       (Dom.Event.make event)
       (Dom.handler (fun ev ->
          handler ev;
          Js._true))
       Js._false
     : Dom.event_listener_id)
;;

let widget_id : (State.t * Dom_html.canvasElement Js.t) Type_equal.Id.t =
  Type_equal.Id.create ~name:"jsip-heap-canvas" (fun (_ : State.t * _) ->
    Sexp.Atom "jsip-heap-canvas")
;;

let init (input : Input.t) () =
  let document = Dom_html.document in
  let canvas = Dom_html.createCanvas document in
  canvas##.style##.cssText
  := Js.string
       "position:absolute;inset:0;width:100%;height:100%;display:block;cursor:grab;touch-action:none";
  let context = canvas##getContext Dom_html._2d_ in
  let state =
    { State.input
    ; canvas
    ; context
    ; dpr = 1.
    ; width = 0.
    ; height = 0.
    ; view = { View_state.x = 0.; y = 0.; k = 1.; k_target = 1. }
    ; tier_f = Heap_layout.tier_for ~k:1.
    ; anchor = None
    ; drag = None
    ; drag_moved = false
    ; mini_drag = false
    ; cursor = None
    ; hover = None
    ; pulses = []
    ; styles = String.Table.create ()
    ; dirty = true
    ; mini = None
    ; raf = None
    ; observer = None
    ; keydown = None
    ; hud_zoom = -1
    ; hud_tier = -1.
    ; fitted = false
    }
  in
  rebuild_styles state;
  listen canvas "wheel" (fun ev -> on_wheel state (Js.Unsafe.coerce ev));
  listen canvas "pointerdown" (fun ev ->
    on_pointer_down state (Js.Unsafe.coerce ev));
  listen canvas "pointermove" (fun ev ->
    on_pointer_move state (Js.Unsafe.coerce ev));
  listen canvas "pointerup" (fun ev ->
    on_pointer_up state (Js.Unsafe.coerce ev));
  listen canvas "pointerleave" (fun ev ->
    on_pointer_leave state (Js.Unsafe.coerce ev));
  listen canvas "dblclick" (fun ev ->
    on_double_click state (Js.Unsafe.coerce ev));
  state.keydown
  <- Some
       (Dom.addEventListener
          document
          Dom_html.Event.keydown
          (Dom.handler (fun ev ->
             on_key state ev;
             Js._true))
          Js._false);
  let observer =
    Js_of_ocaml.ResizeObserver.observe
      ~node:(canvas :> Dom.node Js.t)
      ~f:(fun (_ : _) (_ : _) -> resize state)
      ()
  in
  state.observer <- Some observer;
  (* fonts arriving re-measure nothing (sizes are char counts), but the
     glyphs themselves need one more paint *)
  ignore
    (Dom_html.setTimeout
       (fun () ->
         resize state;
         state.dirty <- true)
       400.
     : Dom_html.timeout_id_safe);
  loop state 0.;
  state, canvas
;;

let update
  (previous : State.t)
  (canvas : Dom_html.canvasElement Js.t)
  (input : Input.t)
  =
  let old = previous.input in
  previous.input <- input;
  (match phys_equal old.roots input.roots with
   | true -> ()
   | false ->
     rebuild_styles previous;
     (* a committed jump or a step rebuilt the scene under the anchor *)
     previous.anchor <- None);
  (match
     Bool.equal old.filter_active input.filter_active
     && Theme.equal old.theme input.theme
   with
   | true -> ()
   | false -> rebuild_styles previous);
  (match old.land_seq = input.land_seq with
   | true -> ()
   | false ->
     let time = now () in
     previous.pulses
     <- List.map input.pulse_ids ~f:(fun id -> { Pulse.id; started = time })
        @ previous.pulses;
     land_on_current previous);
  (match old.focus_seq = input.focus_seq with
   | true -> ()
   | false -> focus_on_target previous);
  (* the slider moved every structure, so the view it was framing is gone —
     refit rather than leave the eye somewhere arbitrary in the new grid *)
  (match old.columns = input.columns with
   | true -> ()
   | false -> fit previous ~min_tier:0.);
  previous.dirty <- true;
  previous, canvas
;;

let destroy (state : State.t) (_ : Dom_html.canvasElement Js.t) =
  (match state.raf with
   | Some id -> Dom_html.window##cancelAnimationFrame id
   | None -> ());
  (match state.observer with
   | Some observer -> observer##disconnect
   | None -> ());
  match state.keydown with
  | Some id -> Dom.removeEventListener id
  | None -> ()
;;

let view (input : Input.t) =
  Vdom.Node.widget
    ~id:widget_id
    ~init:(init input)
    ~update:(fun state canvas -> update state canvas input)
    ~destroy
    ()
;;
