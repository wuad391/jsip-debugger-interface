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
   steps proportionally. Returns [(step, cell_width)] pairs whose widths sum
   to at most [width - 2]. *)
let tick_cells ~width ~total =
  let width = max 1 (width - 2) in
  match total <= width with
  | true ->
    let cell = max 1 (width / total) in
    List.init total ~f:(fun index -> index, cell)
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
  scan (tick_cells ~width ~total) ~column:1
;;

let bar ~cell_width =
  let gap = cell_width > 2 in
  String.concat
    (List.init cell_width ~f:(fun i ->
       match gap && i = cell_width - 1 with true -> " " | false -> "━"))
;;

let ticks ~width ~step ~total =
  let views =
    List.map (tick_cells ~width ~total) ~f:(fun (cell_step, cell_width) ->
      let color =
        match Ordering.of_int (compare cell_step step) with
        | Equal -> Theme.highlight
        | Less -> Theme.tick_past
        | Greater -> Theme.hairline
      in
      View.text ~attrs:(Theme.fg' color) (bar ~cell_width))
  in
  Panel.fit (View.hcat (View.text " " :: views)) ~width ~height:1
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
    ~bg:Theme.panel_bg
    (View.vcat
       [ ticks ~width ~step ~total
       ; controls ~width ~playing
       ; Panel.horizontal_rule ~width ~color:Theme.border
       ])
;;
