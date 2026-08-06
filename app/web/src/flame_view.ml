open! Core
open Jsip_types
open Jsip_web_components
module Vdom = Virtual_dom.Vdom
open Vdom.Html_syntax

(* A flame graph of an instrumented trace is SHALLOW — the events are
   bindings inside functions, not a sampled stack — so the drawer's rows
   spread to fill it rather than sitting in a thin strip along the bottom of
   an empty box. Three rows of forty pixels reads; three of eighteen, with a
   hundred pixels of nothing above them, reads as broken. *)
let drawer_height = 180.
let min_row = 18.
let max_row = 44.

let row_height ~depth_count =
  match depth_count with
  | 0 -> min_row
  | depth_count ->
    Float.clamp_exn
      ((drawer_height -. 34.) /. Float.of_int depth_count)
      ~min:min_row
      ~max:max_row
;;

let segment_view theme (segment : Flame_math.Segment.t) ~depth ~row ~inject =
  let bottom = Float.of_int depth *. row in
  let height = row -. 1. in
  match segment with
  | Flame_math.Segment.Pool { x; width; count } ->
    let x = sprintf "%.1f" x in
    let width = sprintf "%.1f" width in
    let bottom = sprintf "%.1f" bottom in
    let height = sprintf "%.1f" height in
    {%html|<div %{Styles.flame_pool theme ~x ~width ~bottom ~height}>+%{count#Int}</div>|}
  | Flame_math.Segment.Bar
      { path; label; detail; x; width; share; lit; deepest } ->
    let fill, ink =
      match share with
      | None -> Theme.flame_neutral, Theme.flame_label_neutral
      | Some share -> Theme.flame ~share, Theme.flame_label
    in
    let title =
      Vdom.Attr.create
        "title"
        [%string "%{detail} — click jumps, double-click zooms"]
    in
    (* a bar stands for every call that merged into it; clicking goes to the
       first of them — the TUI's jump — and double-click rescales *)
    let jump (_ : _) = inject (Action.Jump_flame path) in
    let zoom (_ : _) = inject (Action.Zoom_flame path) in
    let x = sprintf "%.1f" x in
    let width = sprintf "%.1f" width in
    let bottom = sprintf "%.1f" bottom in
    let height = sprintf "%.1f" height in
    {%html|
      <div %{Styles.flame_bar ~x ~width ~bottom ~height ~fill ~ink ~lit
              ~deepest}
           %{title} on_click=%{jump} on_double_click=%{zoom}>#{label}</div>
    |}
;;

let view
  ~theme
  ~(tree : Flame_tree.t)
  ~rows
  ~open_
  ~zoomed
  ~depth_count
  ~inject
  =
  let title = match open_ with true -> "▾ FLAME" | false -> "▸ FLAME" in
  let source =
    match Flame_math.heat_source tree with
    | `Compute -> "color = compute"
    | `Calls -> "color = calls"
  in
  let zoom_note =
    match zoomed with false -> "" | true -> " · zoomed · Z reset"
  in
  let meta =
    [%string
      "%{tree.total_events#Int} events · width = calls · \
       %{source}%{zoom_note}"]
  in
  let header =
    {%html|
      <div %{Styles.pane_header theme ~clickable:true}
           on_click=%{fun _ -> inject Action.Toggle_flame}>
        <span %{Styles.pane_title theme}>#{title}</span>
        <span %{Styles.pane_meta theme}>#{meta}</span>
      </div>
    |}
  in
  match open_ with
  | false ->
    {%html|<div %{Styles.flame_drawer theme ~open_:false}>%{header}</div>|}
  | true ->
    let row = row_height ~depth_count in
    let bars =
      List.concat_map rows ~f:(fun (flame_row : Flame_math.Row.t) ->
        List.map
          flame_row.segments
          ~f:(segment_view theme ~depth:flame_row.depth ~row ~inject))
    in
    let height = sprintf "%.0fpx" (Float.of_int depth_count *. row) in
    {%html|
      <div %{Styles.flame_drawer theme ~open_:true}>
        %{header}
        <div %{Styles.flame_body theme}>
          <div %{Styles.flame_rows ~height}>*{bars}</div>
        </div>
      </div>
    |}
;;
