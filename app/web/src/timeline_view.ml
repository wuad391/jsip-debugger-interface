open! Core
open Jsip_web_components
module Vdom = Virtual_dom.Vdom
open Vdom.Html_syntax
module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html

(* the fraction across an element a click landed at, off its own box *)
let fraction_of_click (event : Dom_html.mouseEvent Js.t) =
  Js.Opt.case
    event##.currentTarget
    (fun () -> None)
    (fun target ->
      Js.Opt.case
        (Dom_html.CoerceTo.element target)
        (fun () -> None)
        (fun element ->
          let rect = element##getBoundingClientRect in
          let left = Js.to_float rect##.left in
          let width = Js.to_float rect##.right -. left in
          match Float.( > ) width 0. with
          | false -> None
          | true -> Some ((Js.to_float event##.clientX -. left) /. width)))
;;

(* Position is the boundary between the two halves of the strip, capped by
   the segment it stands in — a separate needle floating over that read as a
   second, disagreeing cursor.

   Behind the position the strip is one flat blue: how far in you are is a
   LENGTH, and shading it by density made the answer depend on what the run
   happened to allocate. Ahead of it the density shape survives, sunk toward
   the strip — that is the part worth previewing. *)
let future_sink = 0.74

let ticks ~(theme : Theme.t) ~segments ~step ~total ~inject =
  let count = Array.length segments in
  let played = Timeline_model.played ~total ~step ~segments:count in
  let cursor = Timeline_model.cursor ~total ~step ~segments:count in
  let cells =
    List.init count ~f:(fun index ->
      let value = segments.(index) in
      let color =
        match index = cursor, index < played with
        | true, (true | false) -> theme.accent_bright
        | false, true -> theme.progress
        | false, false ->
          Theme.mix
            (Theme.heat_color theme value)
            theme.strip_bg
            ~amount:future_sink
      in
      {%html|<div %{Styles.tick color}></div>|})
  in
  let on_click event =
    match fraction_of_click event with
    | None -> Vdom.Effect.Ignore
    | Some fraction ->
      inject
        (Action.Step_to (Timeline_model.step_of_fraction ~total ~fraction))
  in
  {%html|
    <div %{Styles.ticks theme} on_click=%{on_click}>
      *{cells}
    </div>
  |}
;;

(* The key legend is also the buttons: every chip is clickable and the ones
   naming a mode light while it is on — the TUI transport's contract, with
   the design's look. *)
let hints
  ~(theme : Theme.t)
  ~playing
  ~accordion
  ~sort_by_address
  ~typing_filter
  ~lod
  ~theme_mode
  ~flame_open
  ~inject
  =
  let chip ?(lit = false) label action =
    let color = match lit with true -> theme.gold | false -> theme.dim in
    {%html|<span %{Styles.hint_chip color} on_click=%{fun _ -> inject action}>#{label}</span>|}
  in
  let accent_chip label action =
    {%html|<span %{Styles.hint_chip theme.accent} on_click=%{fun _ -> inject action}>#{label}</span>|}
  in
  let dot = {%html|<span %{Styles.hint_dot theme}>·</span>|} in
  let model_label =
    match (lod : Action.Lod.t) with
    | Uniform -> "m focal"
    | Focal -> "m uniform"
  in
  let theme_label =
    match (theme_mode : Action.Theme_mode.t) with
    | Dark -> "t light"
    | Light -> "t dark"
  in
  let all =
    [ chip "◄ back" (Action.Step_delta (-1))
    ; chip "step ►" (Action.Step_delta 1)
    ; chip
        ~lit:playing
        (match playing with
         | true -> "[space] pause"
         | false -> "[space] play")
        Action.Toggle_play
    ; chip ". focus" Action.Focus_latest
    ; chip ~lit:accordion "z accordion" Action.Toggle_accordion
    ; chip ~lit:sort_by_address "o by address" Action.Toggle_address_order
    ; chip ~lit:typing_filter "/ filter" Action.Begin_filter
    ; chip ~lit:flame_open "f flame" Action.Toggle_flame
    ; accent_chip model_label Action.Toggle_lod
    ; chip theme_label Action.Toggle_theme
    ; chip "q quit" Action.Quit
    ]
    |> List.intersperse ~sep:dot
  in
  {%html|<div %{Styles.hints theme}>*{all}</div>|}
;;

let view
  ~theme
  ~segments
  ~step
  ~total
  ~playing
  ~accordion
  ~sort_by_address
  ~typing_filter
  ~lod
  ~theme_mode
  ~flame_open
  ~inject
  =
  {%html|
    <div %{Styles.strip theme}>
      %{ticks ~theme ~segments ~step ~total ~inject}
      %{hints ~theme ~playing ~accordion ~sort_by_address ~typing_filter
          ~lod ~theme_mode ~flame_open ~inject}
    </div>
  |}
;;
