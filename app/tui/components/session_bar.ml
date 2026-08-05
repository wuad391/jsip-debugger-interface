open! Core
module View = Bonsai_term.View

let divider = View.text ~attrs:(Theme.fg' Theme.border) " │ "

(* what the stack pane's colored callee names mean: the ramp's colors carry
   each function's share — of the perf profile's sampled compute when one was
   loaded, of the trace's own events otherwise. The label says which, so the
   same colors never silently change meaning. *)
let heat_legend source =
  let label =
    match source with `Compute -> "compute" | `Calls -> "calls"
  in
  View.hcat
    ([ View.text ~attrs:(Theme.fg' Theme.muted) "heat " ]
     @ List.map (Array.to_list Theme.heat_ramp) ~f:(fun color ->
       View.text ~attrs:(Theme.fg' color) "█")
     @ [ View.text ~attrs:(Theme.fg' Theme.muted) [%string " %{label} "] ])
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
    | None -> [ left ]
    | Some source ->
      (* right-aligned, and only when it fits clear of the left run *)
      let legend = heat_legend source in
      let legend_width = View.width legend in
      (match View.width left + legend_width <= width with
       | false -> [ left ]
       | true -> [ left; View.pad ~l:(width - legend_width) legend ])
  in
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.bg
    (Panel.fit (View.zcat layers) ~width ~height:1)
;;
