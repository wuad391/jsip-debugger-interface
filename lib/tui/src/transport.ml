open! Core
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Button = struct
  type t =
    | Back
    | Step
    | Play
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

(* a half-height bar hugging the top edge: heavier than a hairline, and it
   leaves the bottom of its row clear *)
let ticks ~width ~step ~total =
  let row =
    let cells = tick_cells ~width ~total in
    let last = List.length cells - 1 in
    let views =
      List.mapi cells ~f:(fun index (cell_step, cell_width) ->
        let color =
          match Ordering.of_int (Int.compare cell_step step) with
          | Equal -> Theme.highlight
          | Less -> Theme.tick_past
          | Greater -> Theme.hairline
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
let segments ~playing =
  let play_label =
    match playing with true -> "[space] pause" | false -> "[space] play"
  in
  [ Some Button.Back, "◂ back"
  ; None, "  ·  "
  ; Some Button.Step, "step ▸"
  ; None, "  ·  "
  ; Some Button.Play, play_label
  ; None, "  ·  "
  ; Some Button.Quit, "q quit"
  ]
;;

(* display columns, not bytes — ◂ ▸ · are multi-byte glyphs *)
let segment_columns text = View.width (View.text text)

let start_column ~width ~playing =
  let total =
    List.sum
      (module Int)
      (segments ~playing)
      ~f:(fun ((_ : Button.t option), text) -> segment_columns text)
  in
  max 0 (width - total - 1)
;;

let controls ~width ~playing =
  let chips =
    List.map (segments ~playing) ~f:(fun (button, text) ->
      let attrs =
        match button with
        | None -> Theme.fg' Theme.ghost
        | Some Button.Play when playing ->
          [ Theme.fg Theme.highlight; Attr.bold ]
        | Some (Button.Back | Button.Step | Button.Play | Button.Quit) ->
          Theme.fg' Theme.secondary
      in
      View.text ~attrs text)
  in
  Panel.fit
    (View.hcat
       (View.transparent_rectangle
          ~width:(start_column ~width ~playing)
          ~height:1
        :: chips))
    ~width
    ~height:1
;;

let control_at ~width ~playing ~x =
  let (_ : int), hit =
    List.fold
      (segments ~playing)
      ~init:(start_column ~width ~playing, None)
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

let view ~width ~step ~total ~playing =
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.bg
    (Panel.fit
       (View.vcat [ ticks ~width ~step ~total; controls ~width ~playing ])
       ~width
       ~height:Layout.strip_height)
;;
