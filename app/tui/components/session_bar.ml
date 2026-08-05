open! Core
module View = Bonsai_term.View

let divider = View.text ~attrs:(Theme.fg' Theme.border) " │ "

(* What the stack pane's colored callee names mean: the ramp's colors carry
   each function's share — of the perf profile's sampled compute when one was
   loaded, of the trace's own events otherwise. Labeled [stack] because the
   timeline chip sits beside it and the two ramps answer different questions;
   the second label says which share, so the same colors never silently
   change meaning. *)
let stack_legend source =
  let label =
    match source with `Compute -> "compute" | `Calls -> "calls"
  in
  View.hcat
    ([ View.text ~attrs:(Theme.fg' Theme.muted) "stack " ]
     @ List.map (Array.to_list Theme.heat_ramp) ~f:(fun color ->
       View.text ~attrs:(Theme.fg' color) "█")
     @ [ View.text ~attrs:(Theme.fg' Theme.muted) [%string " %{label}"] ])
;;

(* What the timeline's brightness means: each cell brightens with how much
   its steps allocated, within the hue that already says past or future — the
   confusion this chip exists to end is reading a bright band as "hot code"
   when it is "many allocations". Both ramps are shown in their own hues,
   quiet to burst, with the same half-height glyph the bar itself uses. *)
let timeline_legend =
  let ramp colors =
    View.hcat
      (List.map (Array.to_list colors) ~f:(fun color ->
         View.text ~attrs:(Theme.fg' color) "▀"))
  in
  View.hcat
    [ View.text ~attrs:(Theme.fg' Theme.muted) "timeline "
    ; ramp Theme.tick_past_ramp
    ; View.text " "
    ; ramp Theme.tick_future_ramp
    ; View.text ~attrs:(Theme.fg' Theme.muted) " alloc"
    ]
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
  (* Right-aligned, and only whole chips that fit clear of the left run: the
     full pair first, the stack chip alone on a narrower screen, nothing on a
     narrower one still — a truncated legend teaches the wrong thing. *)
  let separator = View.text ~attrs:(Theme.fg' Theme.border) " · " in
  let candidates =
    match heat with
    | None -> [ View.hcat [ timeline_legend; View.text " " ] ]
    | Some source ->
      [ View.hcat
          [ stack_legend source; separator; timeline_legend; View.text " " ]
      ; View.hcat [ stack_legend source; View.text " " ]
      ]
  in
  let layers =
    match
      List.find candidates ~f:(fun legend ->
        View.width left + View.width legend <= width)
    with
    | None -> [ left ]
    | Some legend -> [ left; View.pad ~l:(width - View.width legend) legend ]
  in
  View.with_colors'
    ~fill_backdrop:true
    ~fg:Theme.text
    ~bg:Theme.bg
    (Panel.fit (View.zcat layers) ~width ~height:1)
;;
