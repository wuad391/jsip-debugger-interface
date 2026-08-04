open! Core
module View = Bonsai_term.View

let divider = View.text ~attrs:(Theme.fg' Theme.border) " │ "

(* what the stack pane's heat cells mean: the ramp's colors carry compute
   share, the glyph height call frequency, the dot "no perf data" *)
let heat_legend =
  View.hcat
    ([ View.text ~attrs:(Theme.fg' Theme.muted) "heat " ]
     @ List.map (Array.to_list Theme.heat_ramp) ~f:(fun color ->
       View.text ~attrs:(Theme.fg' color) "█")
     @ [ View.text ~attrs:(Theme.fg' Theme.muted) " cold→hot  "
       ; View.text ~attrs:(Theme.fg' Theme.ghost) "·"
       ; View.text ~attrs:(Theme.fg' Theme.muted) " no data "
       ])
;;

let view ~width ~dump_name ~structure ~heat =
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
  let layers =
    match heat with
    | false -> [ left ]
    | true ->
      (* right-aligned, and only when it fits clear of the left run *)
      let legend_width = View.width heat_legend in
      (match View.width left + legend_width <= width with
       | false -> [ left ]
       | true -> [ left; View.pad ~l:(width - legend_width) heat_legend ])
  in
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.bg
    (Panel.fit (View.zcat layers) ~width ~height:1)
;;
