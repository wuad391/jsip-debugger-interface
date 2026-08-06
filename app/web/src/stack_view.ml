open! Core
open Jsip_web_components
module Vdom = Virtual_dom.Vdom
open Vdom.Html_syntax

let target_action (target : Stack_rows.Target.t) : Action.t =
  match target with
  | Frame index -> Action.Select_frame index
  | Step step -> Action.Step_to step
  | Toggle call -> Action.Toggle_stack_fold call
  | Expand head -> Action.Toggle_stack_run head
;;

let row_view (theme : Theme.t) (row : Stack_rows.Row.t) ~stripe ~inject =
  let is_selected =
    match row.state with
    | Stack_rows.State.Selected -> true
    | Live | Dimmed -> false
  in
  (* heat rides on the callee's name itself; the selection's wash already
     says where you are, so it keeps plain ink — the TUI's reading *)
  let fn_color, fn_bold =
    match row.state, row.heat with
    | Stack_rows.State.Selected, (_ : float option) ->
      theme.selection_text, true
    | Live, None -> theme.bright, true
    | Live, Some share -> Theme.stack_heat ~share, true
    | Dimmed, None -> theme.ghost, false
    | Dimmed, Some share ->
      Theme.mix (Theme.stack_heat ~share) theme.bg ~amount:0.35, false
  in
  let args_color =
    match row.state with
    | Stack_rows.State.Selected ->
      Theme.mix theme.selection_text theme.selection_bg ~amount:0.25
    | Live -> theme.dim
    | Dimmed -> theme.ghost
  in
  let indent = String.make (2 * (row.depth - 1)) ' ' in
  let glyph_text =
    match row.glyph with
    | Stack_rows.Glyph.Blank -> " "
    | Fold true | Run true -> "▸"
    | Fold false | Run false -> "▾"
  in
  let glyph =
    match row.glyph_target with
    | None -> {%html|<span %{Styles.stack_glyph theme}>#{glyph_text}</span>|}
    | Some target ->
      let on_click (_ : _) =
        Vdom.Effect.Many
          [ Vdom.Effect.Stop_propagation; inject (target_action target) ]
      in
      {%html|<span %{Styles.stack_glyph theme} on_click=%{on_click}>#{glyph_text}</span>|}
  in
  let hidden =
    match row.hidden with
    | Stack_rows.Hidden.Nothing -> Vdom.Node.none
    | Descendants count ->
      {%html|<span %{Styles.stack_hidden theme}> ⋯ %{count#Int}</span>|}
    | Repeats count ->
      {%html|<span %{Styles.stack_hidden theme}> ⋯ ×%{count#Int}</span>|}
  in
  let registered =
    match row.registered with
    | None -> Vdom.Node.none
    | Some label ->
      (* the same voice the heap pane's names speak in, so [#826] here and
         there read as one thing *)
      {%html|<span %{Styles.stack_registered theme}> · #{label}</span>|}
  in
  let id = Vdom.Attr.id [%string "stack-row-%{row.step#Int}"] in
  {%html|
    <div %{id} %{Styles.stack_row theme ~selected:is_selected ~stripe}
         on_click=%{fun _ -> inject (target_action row.target)}>
      <span>#{indent}</span>
      %{glyph}
      <span> </span>
      <span %{Styles.colored ~bold:fn_bold fn_color}>#{row.fn}</span>
      <span %{Styles.color args_color}> #{row.args}</span>
      %{registered}
      %{hidden}
    </div>
  |}
;;

let view ~theme ~rows ~total_calls ~live_count ~has_heat ~collapsed ~inject =
  let title =
    match collapsed with true -> "▸ CALL STACK" | false -> "▾ CALL STACK"
  in
  let heat_meta = match has_heat with true -> " · heat" | false -> "" in
  let meta =
    [%string "%{total_calls#Int} calls · %{live_count#Int} live%{heat_meta}"]
  in
  let header =
    {%html|
      <div %{Styles.pane_header theme ~clickable:true}
           on_click=%{fun _ -> inject Action.Toggle_stack_pane}>
        <span %{Styles.pane_title theme}>#{title}</span>
        <span %{Styles.pane_meta theme}>#{meta}</span>
      </div>
    |}
  in
  match collapsed with
  | true ->
    {%html|<div %{Styles.pane theme ~bordered_bottom:true}>%{header}</div>|}
  | false ->
    let body =
      List.mapi rows ~f:(fun index row ->
        row_view theme row ~stripe:(index % 2 = 1) ~inject)
    in
    {%html|
      <div %{Styles.pane theme ~bordered_bottom:true}>
        %{header}
        <div %{Styles.pane_body} %{Vdom.Attr.id "stack-scroll"}>*{body}</div>
      </div>
    |}
;;
