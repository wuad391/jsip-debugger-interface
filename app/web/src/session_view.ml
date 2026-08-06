open! Core
open Jsip_web_components
module Vdom = Virtual_dom.Vdom
open Vdom.Html_syntax

(* the bottom bar: app chip, dump, the walked structure, the replay mode —
   and the right end is the cross-pane legend, the one place that says what
   the two ambient color scales mean *)
let view ~(theme : Theme.t) ~dump_name ~structure ~playing ~heat =
  let mode = match playing with true -> "playing" | false -> "replay" in
  let stack_legend =
    match (heat : [ `Compute | `Calls ] option) with
    | None -> Vdom.Node.none
    | Some source ->
      let label =
        match source with `Compute -> "compute" | `Calls -> "calls"
      in
      let cells =
        Array.to_list Theme.stack_heat_ramp
        |> List.map ~f:(fun color ->
          {%html|<div %{Styles.legend_cell color}></div>|})
      in
      {%html|
        <>
          <span %{Styles.session_faint theme}>stack</span>
          <div %{Styles.legend_cells}>*{cells}</div>
          <span %{Styles.session_faint theme}>#{label}</span>
        </>
      |}
  in
  let timeline_legend =
    let cells =
      [ 0.1; 0.35; 0.6; 0.82; 1.0 ]
      |> List.map ~f:(fun value ->
        let color = Theme.heat_color theme value in
        {%html|<div %{Styles.legend_cell color}></div>|})
    in
    {%html|
      <>
        <span %{Styles.session_faint theme}>timeline</span>
        <div %{Styles.legend_cells}>*{cells}</div>
        <span %{Styles.session_faint theme}>alloc</span>
      </>
    |}
  in
  {%html|
    <div %{Styles.session theme}>
      <div %{Styles.session_group}>
        <span %{Styles.session_dot theme}>●</span>
        <span %{Styles.session_app theme}>jsip-debug</span>
        <span %{Styles.session_sep theme}>|</span>
        <span>#{dump_name}</span>
        <span %{Styles.session_sep theme}>|</span>
        <span>#{structure}</span>
        <span %{Styles.session_faint theme}>·</span>
        <span %{Styles.session_faint theme}>#{mode}</span>
      </div>
      <div %{Styles.session_right}>
        %{stack_legend}
        %{timeline_legend}
      </div>
    </div>
  |}
;;
