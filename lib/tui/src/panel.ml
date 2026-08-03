open! Core
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

let fit view ~width ~height =
  let cropped =
    View.crop
      ~r:(max 0 (View.width view - width))
      ~b:(max 0 (View.height view - height))
      view
  in
  View.zcat [ cropped; View.transparent_rectangle ~width ~height ]
;;

let repeat glyph ~width =
  String.concat (List.init (max 0 width) ~f:(fun (_ : int) -> glyph))
;;

(* dividers sit on the app surface between panes, so they state that
   background rather than inheriting the terminal's *)
let divider_attrs color = [ Theme.fg color; Attr.bg Theme.bg ]

let horizontal_rule ~width ~color =
  View.text ~attrs:(divider_attrs color) (repeat "─" ~width)
;;

let vertical_rule ~height ~color =
  View.vcat
    (List.init (max 0 height) ~f:(fun (_ : int) ->
       View.text ~attrs:(divider_attrs color) "│"))
;;

let junction ~color = View.text ~attrs:(divider_attrs color) "┤"

let header_height = 1
let inner_width ~width = max 0 (width - 2)

let view ~title ~meta ~width ~height body =
  let title_view =
    View.text ~attrs:[ Theme.fg Theme.secondary ] (String.uppercase title)
  in
  let meta_view = View.text ~attrs:(Theme.fg' Theme.faint) meta in
  let gap =
    max 1 (width - View.width title_view - View.width meta_view - 2)
  in
  let header =
    View.hcat
      [ View.text " "
      ; title_view
      ; View.transparent_rectangle ~width:gap ~height:1
      ; meta_view
      ; View.text " "
      ]
  in
  let body =
    View.pad
      ~l:1
      (fit body ~width:(inner_width ~width) ~height:(height - header_height))
  in
  (* [with_colors'] *sets* the unspecified background on every cell — a
     rectangle behind the pane would only show through the gaps, leaving
     text sitting on the terminal's own background instead of the pane's *)
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.panel_bg
    (fit (View.vcat [ fit header ~width ~height:1; body ]) ~width ~height)
;;
