open! Core
open Jsip_web_components
module Vdom = Virtual_dom.Vdom
open Vdom.Html_syntax

let span_view (theme : Theme.t) (token, text, marked) =
  let color = Theme.of_token theme token in
  let italic =
    match (token : Jsip_parsing.Syntax.Token.t) with
    | Comment -> true
    | Keyword | Uident | String | Number | Operator | Plain -> false
  in
  match marked with
  | false -> {%html|<span %{Styles.colored ~italic color}>#{text}</span>|}
  | true ->
    (* the event's [char_range], marked the mockup's way: a deeper wash and
       an underline *)
    let attr =
      Vdom.Attr.create
        "style"
        [%string
          "color:%{theme.selection_text};background:%{theme.range_bg};border-bottom:1px \
           solid %{theme.range_border}"]
    in
    {%html|<span %{attr}>#{text}</span>|}
;;

let row_view (theme : Theme.t) (row : Source_model.Row.t) ~file ~inject =
  match row with
  | Source_model.Row.Folded_marker { start; stop; hides_active } ->
    let count = stop - start in
    let attr = Styles.source_line theme ~active:hides_active in
    let on_click (_ : _) =
      inject
        (Action.Toggle_source_fold { Action.Source_fold.file; line = start })
    in
    {%html|
      <div %{attr}>
        <span %{Styles.source_fold_gutter theme ~foldable:false}> </span>
        <span %{Styles.source_number theme ~active:false}> </span>
        <span %{Styles.source_folded_marker theme} on_click=%{on_click}>⋯ %{count#Int} lines</span>
      </div>
    |}
  | Source_model.Row.Code
      { number; is_active; is_callsite; region_start; folded; spans } ->
    let gutter_glyph =
      match region_start, folded, is_callsite with
      | true, true, (_ : bool) -> "▸"
      | true, false, (_ : bool) -> "▾"
      | false, (_ : bool), true -> "▸"
      | false, (_ : bool), false -> ""
    in
    let gutter =
      match region_start with
      | false ->
        {%html|<span %{Styles.source_fold_gutter theme ~foldable:false}>#{gutter_glyph}</span>|}
      | true ->
        let on_click (_ : _) =
          Vdom.Effect.Many
            [ Vdom.Effect.Stop_propagation
            ; inject
                (Action.Toggle_source_fold
                   { Action.Source_fold.file; line = number })
            ]
        in
        {%html|<span %{Styles.source_fold_gutter theme ~foldable:true} on_click=%{on_click}>#{gutter_glyph}</span>|}
    in
    let code = List.map spans ~f:(span_view theme) in
    let id = Vdom.Attr.id [%string "src-line-%{number#Int}"] in
    {%html|
      <div %{id} %{Styles.source_line theme ~active:is_active}>
        %{gutter}
        <span %{Styles.source_number theme ~active:is_active}>%{number#Int}</span>
        <span %{Styles.source_code}>*{code}</span>
      </div>
    |}
;;

let view ~theme ~source ~file_label ~file ~line_count ~collapsed ~inject =
  let title =
    match collapsed with true -> "▸ SOURCE" | false -> "▾ SOURCE"
  in
  let meta = [%string "%{file_label} · %{line_count#Int} lines"] in
  let header =
    {%html|
      <div %{Styles.pane_header theme ~clickable:true}
           on_click=%{fun _ -> inject Action.Toggle_source_pane}>
        <span %{Styles.pane_title theme}>#{title}</span>
        <span %{Styles.pane_meta theme}>#{meta}</span>
      </div>
    |}
  in
  match collapsed with
  | true ->
    {%html|<div %{Styles.pane theme ~bordered_bottom:false}>%{header}</div>|}
  | false ->
    let body =
      match source with
      | Ok rows -> List.map rows ~f:(row_view theme ~file ~inject)
      | Error error ->
        (* a missing file is a placeholder pane, not a crash — where it
           looked and which flag moves the search *)
        [ {%html|<div %{Styles.source_folded_marker theme}> #{Error.to_string_hum error}</div>|}
        ]
    in
    {%html|
      <div %{Styles.pane theme ~bordered_bottom:false}>
        %{header}
        <div %{Styles.pane_body theme} %{Vdom.Attr.id "source-scroll"}>*{body}</div>
      </div>
    |}
;;
