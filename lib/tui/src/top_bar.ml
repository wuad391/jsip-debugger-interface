open! Core
module View = Bonsai_term.View

let divider = View.text ~attrs:(Theme.fg' Theme.border) " │ "

let label text =
  View.text ~attrs:(Theme.fg' Theme.faint) (String.uppercase text)
;;

let view ~width ~dump_name ~structure ~phase ~step ~total =
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
  let right =
    View.hcat
      [ label "phase "
      ; View.text ~attrs:(Theme.fg' Theme.secondary) (String.uppercase phase)
      ; divider
      ; label "step "
      ; View.text ~attrs:(Theme.fg' Theme.text) (Int.to_string step)
      ; View.text ~attrs:(Theme.fg' Theme.faint) [%string "/%{total#Int}"]
      ; View.text " "
      ]
  in
  let gap = max 1 (width - View.width left - View.width right) in
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.panel_bg
    (Panel.fit
       (View.hcat
          [ left; View.transparent_rectangle ~width:gap ~height:1; right ])
       ~width
       ~height:1)
;;
