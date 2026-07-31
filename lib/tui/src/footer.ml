open! Core
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Button = struct
  type t =
    | Back
    | Step
    | Play
  [@@deriving sexp_of, equal]
end

(* mockup tick strip: one segment per step when they fit, otherwise cells
   share steps proportionally. Returns [(step, cell_width)] pairs whose
   widths sum to at most [width - 2]. *)
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
        | Equal -> Theme.accent
        | Less -> Theme.tick_past
        | Greater -> Theme.hairline
      in
      View.text ~attrs:(Theme.fg' color) (bar ~cell_width))
  in
  Panel.fit (View.hcat (View.text " " :: views)) ~width ~height:1
;;

let controls ~width ~playing =
  let chip ?(attrs = Theme.fg' Theme.text) text =
    View.hcat [ View.text ~attrs text; View.text "  " ]
  in
  let play_attrs =
    match playing with
    | true -> [ Theme.fg Theme.accent_deep; Attr.bold ]
    | false -> Theme.fg' Theme.text
  in
  let play_label =
    match playing with true -> "⏸ pause" | false -> "⏵ play"
  in
  let left =
    View.hcat
      [ View.text " "
      ; chip "◂ back"
      ; chip "step ▸"
      ; chip ~attrs:play_attrs play_label
      ]
  in
  let hints =
    View.text
      ~attrs:(Theme.fg' Theme.faint)
      "◂ ▸ step · space play · ↑ ↓ frame · click jumps · q quit "
  in
  let gap = max 1 (width - View.width left - View.width hints) in
  Panel.fit
    (View.hcat
       [ left; View.transparent_rectangle ~width:gap ~height:1; hints ])
    ~width
    ~height:1
;;

(* x extents of each chip on the controls row; matches [controls]'s layout ("
   " ^ "◂ back " ^ "step ▸ " ^ play label) *)
let button_at ~x =
  let chips =
    [ Button.Back, 1, 7; Button.Step, 9, 15; Button.Play, 17, 24 ]
  in
  List.find_map chips ~f:(fun (button, start, stop) ->
    match x >= start && x <= stop with true -> Some button | false -> None)
;;

let view ~width ~step ~total ~playing =
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.panel_bg
    (View.vcat
       [ Panel.horizontal_rule ~width ~color:Theme.border
       ; ticks ~width ~step ~total
       ; controls ~width ~playing
       ])
;;
