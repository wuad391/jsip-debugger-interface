open! Core
module View = Bonsai_term.View

let divider = View.text ~attrs:(Theme.fg' Theme.border) " │ "

let view ~width ~dump_name ~structure =
  let left =
    View.hcat
      [ View.text " "
      ; View.text ~attrs:(Theme.fg' Theme.accent) "● "
      ; View.text ~attrs:(Theme.fg' Theme.app_purple) "ocaml-debug"
      ; divider
      ; View.text ~attrs:(Theme.fg' Theme.text) dump_name
      ; divider
      ; View.text
          ~attrs:(Theme.fg' Theme.muted)
          [%string "%{structure} · replay"]
      ]
  in
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.bg
    (Panel.fit left ~width ~height:1)
;;
