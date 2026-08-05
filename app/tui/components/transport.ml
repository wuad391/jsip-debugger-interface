open! Core
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

(* what the flame drawer is doing, as far as the chip row cares: shut, open,
   or open and holding the keyboard — the last one rebinds [z], so the legend
   has to change with it *)
module Flame_state = struct
  type t =
    | Shut
    | Open
    | Focused
  [@@deriving sexp_of, equal]

  let is_open t = match t with Shut -> false | Open | Focused -> true
end

module Button = struct
  type t =
    | Back
    | Step
    | Play
    | Fold
    | Accordion
    | Filter
    | Flame
    | Zoom
    | Reset_zoom
    | Quit
  [@@deriving sexp_of, equal]
end

(* tick strip: one segment per step when they fit, otherwise cells share
   steps proportionally. The cells tile [width] exactly — each boundary is
   computed from the screen width rather than from a rounded-down cell size,
   so the remainder is spread through the bar instead of being left as a stub
   at the right edge, and the bar runs flush to both ends. Returns
   [(step, cell_width)] pairs whose widths sum to exactly [width]. *)
let tick_cells ~width ~total =
  let width = max 1 width in
  match total <= width with
  | true ->
    List.init total ~f:(fun index ->
      index, ((index + 1) * width / total) - (index * width / total))
  | false -> List.init width ~f:(fun x -> x * total / width, 1)
;;

let step_at ~width ~total ~x =
  let rec scan cells ~column =
    match cells with
    | [] -> None
    | (step, cell_width) :: rest ->
      let stop = column + cell_width in
      (match x >= column && x < stop with
       | true -> Some step
       | false -> scan rest ~column:stop)
  in
  scan (tick_cells ~width ~total) ~column:0
;;

(* cells are separated by a one-column gap, but the last one runs to the edge
   — a trailing gap there would read as the bar falling short *)
let bar ~cell_width ~is_last =
  let gap = (not is_last) && cell_width > 2 in
  String.concat
    (List.init cell_width ~f:(fun i ->
       match gap && i = cell_width - 1 with true -> " " | false -> "▀"))
;;

(* the busiest step a cell covers — a cell spans from its own step to the
   next cell's; a burst inside the span should light the whole cell, so max,
   not mean *)
let cell_density ~density ~lo ~hi =
  let hi = min hi (Array.length density) in
  let rec go i acc =
    match i < hi with
    | false -> acc
    | true -> go (i + 1) (Float.max acc density.(i))
  in
  match lo < hi with true -> go lo 0.0 | false -> 0.0
;;

(* a half-height bar hugging the top edge: heavier than a hairline, and it
   leaves the bottom of its row clear. Past and future cells brighten with
   the activity they cover, so the run's busy phases read straight off the
   bar; the current cell keeps the flat highlight. *)
let ticks ~width ~step ~total ~density =
  let row =
    let cells = tick_cells ~width ~total in
    let last = List.length cells - 1 in
    let starts = Array.of_list (List.map cells ~f:fst) in
    let views =
      List.mapi cells ~f:(fun index (cell_step, cell_width) ->
        let cell_stop =
          match index < Array.length starts - 1 with
          | true -> starts.(index + 1)
          | false -> total
        in
        let density = cell_density ~density ~lo:cell_step ~hi:cell_stop in
        let color =
          match Ordering.of_int (Int.compare cell_step step) with
          | Equal -> Theme.highlight
          | Less -> Theme.tick_density Theme.tick_past_ramp ~density
          | Greater -> Theme.tick_density Theme.tick_future_ramp ~density
        in
        View.text
          ~attrs:(Theme.fg' color)
          (bar ~cell_width ~is_last:(index = last)))
    in
    Panel.fit (View.hcat views) ~width ~height:1
  in
  View.vcat (List.init Layout.tick_height ~f:(fun (_ : int) -> row))
;;

(* The chips, right-aligned. Each chip names its key, so the row is both the
   controls and the whole key legend — the one place control info lives — and
   every chip is clickable. Layout math lives here so the view and
   [control_at] can never disagree. *)
let segments ~playing ~flame =
  let play_label =
    match playing with true -> "[space] pause" | false -> "[space] play"
  in
  let dot = None, " · " in
  (* focus rebinds [z] — accordion in the heap, zoom in the flame drawer — so
     the middle of the row swaps with it. A legend naming keys that do not
     work is worse than no legend. *)
  let middle =
    match (flame : Flame_state.t) with
    | Shut | Open ->
      [ Some Button.Fold, "h fold"
      ; dot
      ; Some Button.Accordion, "z accordion"
      ]
    | Focused ->
      [ Some Button.Zoom, "z zoom"; dot; Some Button.Reset_zoom, "Z reset" ]
  in
  let middle =
    middle
    @ [ dot
      ; Some Button.Filter, "/ filter"
      ; dot
      ; Some Button.Flame, "f flame"
      ]
  in
  [ Some Button.Back, "◂ back"
  ; dot
  ; Some Button.Step, "step ▸"
  ; dot
  ; Some Button.Play, play_label
  ; dot
  ]
  @ middle
  @ [ dot; Some Button.Quit, "q quit" ]
;;

(* display columns, not bytes — ◂ ▸ · are multi-byte glyphs *)
let segment_columns text = View.width (View.text text)

let start_column ~width ~playing ~flame =
  let total =
    List.sum
      (module Int)
      (segments ~playing ~flame)
      ~f:(fun ((_ : Button.t option), text) -> segment_columns text)
  in
  max 0 (width - total - 1)
;;

let controls ~width ~playing ~accordion ~flame =
  let chips =
    List.map (segments ~playing ~flame) ~f:(fun (button, text) ->
      (* the chips that name a mode light up while it is on, the same cue for
         both: you can read the row as state, not just as keys *)
      let attrs =
        match button with
        | None -> Theme.fg' Theme.ghost
        | Some Button.Play when playing ->
          [ Theme.fg Theme.highlight; Attr.bold ]
        | Some Button.Accordion when accordion ->
          [ Theme.fg Theme.highlight; Attr.bold ]
        | Some Button.Flame when Flame_state.is_open flame ->
          [ Theme.fg Theme.highlight; Attr.bold ]
        | Some
            ( Button.Back | Button.Step | Button.Play | Button.Fold
            | Button.Accordion | Button.Filter | Button.Flame | Button.Zoom
            | Button.Reset_zoom | Button.Quit ) ->
          Theme.fg' Theme.secondary
      in
      View.text ~attrs text)
  in
  Panel.fit
    (View.hcat
       (View.transparent_rectangle
          ~width:(start_column ~width ~playing ~flame)
          ~height:1
        :: chips))
    ~width
    ~height:1
;;

let control_at ~width ~playing ~flame ~x =
  let (_ : int), hit =
    List.fold
      (segments ~playing ~flame)
      ~init:(start_column ~width ~playing ~flame, None)
      ~f:(fun (column, hit) (button, text) ->
        let stop = column + segment_columns text in
        let hit =
          match button with
          | Some button when x >= column && x < stop -> Some button
          | Some _ | None -> hit
        in
        stop, hit)
  in
  hit
;;

let view ~width ~step ~total ~density ~playing ~accordion ~flame =
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.bg
    (Panel.fit
       (View.vcat
          [ ticks ~width ~step ~total ~density
          ; controls ~width ~playing ~accordion ~flame
          ])
       ~width
       ~height:Layout.strip_height)
;;
