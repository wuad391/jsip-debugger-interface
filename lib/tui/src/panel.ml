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

let horizontal_rule ~width ~color =
  View.text ~attrs:(Theme.fg' color) (repeat "─" ~width)
;;

let inner_width ~width = max 0 (width - 4)

let view ?(strong = false) ~title ~meta ~width ~height body =
  let border_color =
    match strong with true -> Theme.border_strong | false -> Theme.border
  in
  let border_attrs = Theme.fg' border_color in
  let title_view =
    View.text ~attrs:[ Theme.fg Theme.secondary ] (String.uppercase title)
  in
  let meta_view = View.text ~attrs:(Theme.fg' Theme.faint) meta in
  let fill =
    max 0 (width - View.width title_view - View.width meta_view - 6)
  in
  let top =
    View.hcat
      [ View.text ~attrs:border_attrs "┌ "
      ; title_view
      ; View.text ~attrs:border_attrs (" " ^ repeat "─" ~width:fill)
      ; View.text " "
      ; meta_view
      ; View.text ~attrs:border_attrs " ┐"
      ]
  in
  let bottom =
    View.hcat
      [ View.text ~attrs:border_attrs "└"
      ; horizontal_rule ~width:(max 0 (width - 2)) ~color:border_color
      ; View.text ~attrs:border_attrs "┘"
      ]
  in
  let side =
    View.vcat
      (List.init
         (max 0 (height - 2))
         ~f:(fun _ -> View.text ~attrs:border_attrs "│"))
  in
  let padded_body =
    View.pad ~l:1 (fit body ~width:(inner_width ~width) ~height:(height - 2))
  in
  let framed =
    View.vcat
      [ fit top ~width ~height:1
      ; View.hcat
          [ side
          ; fit padded_body ~width:(width - 2) ~height:(height - 2)
          ; side
          ]
      ; bottom
      ]
  in
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.panel_bg
    (fit framed ~width ~height)
;;
