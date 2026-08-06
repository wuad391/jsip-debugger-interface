open! Core
open Jsip_types
open Jsip_web_components
module Vdom = Virtual_dom.Vdom
open Vdom.Html_syntax

(* The heap pane's OUTLINE tab: {!Heap_outline}'s rows, drawn. One line per
   row — guides, fold glyph, then the columns two spaces apart, the way the
   TUI pane draws them — over the canvas the DIAGRAM tab shows.

   The pane's marks are the canvas's, so the two tabs cannot disagree about
   what is chosen: the pinned box's blue wash (every drawing of it, so a [↗]
   lights beside the row it points at), the walked structure's orange edge,
   the [new] green, and a fade for anything the program can no longer name or
   the filter did not find. Clicking a row is clicking its box: jump to where
   it was allocated, and pin it. Clicking the glyph folds, and the fold is
   the same box's, so folding here shows up over there. *)

(* the id the app scrolls to as the replay steps, so the walked structure
   stays on screen — the TUI's landing, spelled scrollIntoView *)
let current_row_id = "heap-outline-current"

let span_color (theme : Theme.t) (kind : Heap_outline.Span.Kind.t) =
  match kind with
  | Key -> theme.bright
  | Value -> theme.node_value
  | Label -> theme.node_key
  | Arrow -> theme.node_key
  | Null -> theme.edge_label
  | Gap -> theme.text
;;

(* One row's pieces, in drawing order: colored text, and whether it leans.
   The columns that are empty are dropped and the rest are two spaces apart,
   so a row with no field and no name starts at its value. *)
let row_pieces (theme : Theme.t) (row : Heap_outline.Row.t) ~dim =
  let ink color =
    match dim with true -> Theme.fade theme color | false -> color
  in
  let piece ?(italic = false) ?(bold = false) color text =
    match String.is_empty text with
    | true -> []
    | false -> [ ink color, italic, bold, text ]
  in
  let value =
    List.concat_map
      row.value
      ~f:(fun ((kind : Heap_outline.Span.Kind.t), text) ->
        let italic =
          match kind with
          | Null -> true
          | Key | Value | Label | Arrow | Gap -> false
        in
        let bold =
          match kind with
          | Key -> true
          | Value | Label | Arrow | Null | Gap -> false
        in
        piece ~italic ~bold (span_color theme kind) text)
  in
  (* the walked structure keeps its highlight even faded — where the step
     happened outranks how reachable it left things *)
  let name_color =
    match row.is_current with
    | true -> theme.accent_bright
    | false -> theme.accent
  in
  let hidden =
    match row.folded && row.hidden > 0 with
    | false -> ""
    | true -> [%string "⋯ %{row.hidden#Int}"]
  in
  let columns =
    List.filter
      [ piece theme.edge_label row.field
      ; piece ~bold:true name_color row.name
      ; piece theme.dim row.ty
      ; value
      ; piece theme.faint row.stats
      ; piece theme.faint hidden
      ; piece
          theme.fresh
          (match row.is_new with true -> "new" | false -> "")
      ]
      ~f:(fun column -> not (List.is_empty column))
  in
  List.concat
    (List.intersperse columns ~sep:[ ink theme.text, false, false, "  " ])
;;

let row_view
  (theme : Theme.t)
  (row : Heap_outline.Row.t)
  ~selected
  ~lead
  ~inject
  =
  let dim = row.faded || not row.matched in
  let ink color =
    match dim with true -> Theme.fade theme color | false -> color
  in
  (* the glyph column is two cells whether or not this row has one, so the
     rows under a structure line up with each other and with its name *)
  let glyph =
    match Heap_outline.Row.glyph row with
    | None ->
      let blank = "  " in
      {%html|<span %{Styles.colored (ink theme.hairline)}>#{blank}</span>|}
    | Some glyph ->
      (* folding is the one thing a row does that is not selecting it *)
      let on_click (_ : _) =
        Vdom.Effect.Many
          [ Vdom.Effect.Stop_propagation
          ; inject (Action.Toggle_heap_fold row.fold)
          ]
      in
      let glyph = [%string "%{glyph} "] in
      {%html|
        <span %{Styles.outline_glyph (ink theme.accent)}
              on_click=%{on_click}>#{glyph}</span>
      |}
  in
  let pieces =
    List.map
      (row_pieces theme row ~dim)
      ~f:(fun (color, italic, bold, text) ->
        {%html|<span %{Styles.colored ~italic ~bold color}>#{text}</span>|})
  in
  (* the address spells itself out on the row you pinned and nowhere else:
     reserving the margin on every row would cost a sixth of the pane to a
     string that is usually not wanted *)
  let address =
    match selected, row.address with
    | true, Some address ->
      let text = Snapshot.Address.display address in
      {%html|<span %{Styles.outline_address theme}>#{text}</span>|}
    | (true | false), (None | Some _) -> Vdom.Node.none
  in
  let on_click (_ : _) =
    match row.address with
    | None -> Vdom.Effect.Ignore
    | Some address -> inject (Action.Select_heap_address address)
  in
  let id =
    match row.is_current && row.depth = 0 with
    | false -> Vdom.Attr.empty
    | true -> Vdom.Attr.id current_row_id
  in
  {%html|
    <div %{id}
         %{Styles.outline_row theme ~selected ~current:row.is_current ~lead}
         title=%{Heap_outline.Row.text row}
         on_click=%{on_click}>
      <span %{Styles.outline_line}>
        <span %{Styles.colored (ink theme.hairline)}>#{row.guide}</span>
        %{glyph}
        *{pieces}
      </span>
      %{address}
    </div>
  |}
;;

let legend (theme : Theme.t) =
  let word color text =
    {%html|<span %{Styles.colored color}>#{text}</span>|}
  in
  let words =
    [ word theme.accent "name"
    ; word theme.dim "type"
    ; word theme.node_value "value"
    ; word theme.fresh "new"
    ; word theme.edge_label "↗ shared"
    ; word (Theme.fade theme theme.text) "faded=unreachable"
    ]
  in
  {%html|<div %{Styles.outline_legend theme}>*{words}</div>|}
;;

(* [selected] is the pinned address: every drawing of that node wears the
   wash, which is how a [↗] and the row it points at find each other. *)
let view ~theme ~(rows : Heap_outline.Row.t list) ~selected ~inject =
  let body =
    match rows with
    | [] ->
      [ {%html|<div %{Styles.outline_empty theme}>nothing tracked yet</div>|}
      ]
    | rows ->
      List.mapi rows ~f:(fun index (row : Heap_outline.Row.t) ->
        let is_selected =
          match selected, row.address with
          | Some selected, Some address ->
            Snapshot.Address.equal selected address
          | (Some _ | None), (None | Some _) -> false
        in
        row_view
          theme
          row
          ~selected:is_selected
          ~lead:(index > 0 && row.depth = 0)
          ~inject)
  in
  {%html|<div %{Styles.outline_panel theme}>*{body}%{legend theme}</div>|}
;;
