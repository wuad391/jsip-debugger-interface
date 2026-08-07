open! Core
open Jsip_web_components
module Vdom = Virtual_dom.Vdom

(* This project does not depend on [ppx_css], so [style=] inside [{%html}] is
   off limits; styles are plain [style] attributes built here, named for
   their roles, over whichever {!Theme} palette the model holds. *)
let style css = Vdom.Attr.create "style" css

let root (theme : Theme.t) =
  style
    [%string
      "height:100vh;display:grid;grid-template-rows:auto 1fr \
       auto;background:%{theme.bg};color:%{theme.text};font-family:'JetBrains \
       Mono',ui-monospace,SFMono-Regular,Menlo,monospace;font-size:13px;line-height:1.45;overflow:hidden"]
;;

let error_page (theme : Theme.t) =
  style
    [%string
      "min-height:100vh;display:flex;align-items:center;justify-content:center;background:%{theme.bg};color:%{theme.text};font-family:'JetBrains \
       Mono',ui-monospace,monospace;font-size:13px;white-space:pre-wrap;padding:24px"]
;;

(* ── top strip ── *)

let strip (theme : Theme.t) =
  style
    [%string
      "display:grid;grid-template-rows:auto auto;background:%{theme.bg}"]
;;

let ticks (theme : Theme.t) =
  style
    [%string
      "position:relative;display:flex;gap:1px;height:13px;padding:0 \
       2px;background:%{theme.strip_bg};cursor:pointer"]
;;

let tick cell_color = style [%string "flex:1;background:%{cell_color}"]

let hints (theme : Theme.t) =
  style
    [%string
      "display:flex;justify-content:flex-end;align-items:center;flex-wrap:wrap;gap:0;padding:5px \
       16px 7px;color:%{theme.dim};font-size:13px;letter-spacing:.01em"]
;;

let hint_chip color =
  style
    [%string "padding:0 8px;color:%{color};cursor:pointer;user-select:none"]
;;

let hint_dot (theme : Theme.t) = style [%string "color:%{theme.separator}"]

(* ── the pane grid ── *)

let main (theme : Theme.t) =
  style
    [%string
      "display:grid;grid-template-columns:436px \
       1fr;min-height:0;border-top:1px solid %{theme.border}"]
;;

let left_column ~stack_collapsed ~source_collapsed =
  let rows =
    match stack_collapsed, source_collapsed with
    | false, false -> "1.15fr 1fr"
    | true, false -> "auto 1fr"
    | false, true -> "1fr auto"
    | true, true -> "auto auto 1fr"
  in
  style
    [%string
      "display:grid;grid-template-rows:%{rows};min-height:0;min-width:0"]
;;

let pane (theme : Theme.t) ~bordered_bottom =
  let bottom =
    match bordered_bottom with
    | true -> [%string "border-bottom:1px solid %{theme.border};"]
    | false -> ""
  in
  style
    [%string
      "display:grid;grid-template-rows:auto \
       1fr;min-height:0;border-right:1px solid %{theme.border};%{bottom}"]
;;

let pane_header (theme : Theme.t) ~clickable =
  let cursor =
    match clickable with true -> "cursor:pointer;" | false -> ""
  in
  style
    [%string
      "display:flex;justify-content:space-between;align-items:baseline;padding:7px \
       12px 6px;color:%{theme.dim};%{cursor}user-select:none"]
;;

let pane_title (theme : Theme.t) =
  style [%string "color:%{theme.heading};letter-spacing:.09em"]
;;

let pane_meta (theme : Theme.t) =
  style [%string "color:%{theme.faint};font-size:12px"]
;;

(* [scrollbar-color] because the panes are the only scrollers on the page and
   a browser left to itself paints them off the OS theme, not this one — a
   black bar down the side of the light theme *)
let pane_body (theme : Theme.t) =
  style
    [%string
      "overflow-y:auto;overflow-x:hidden;padding:0 0 \
       14px;scrollbar-width:thin;scrollbar-color:%{theme.separator} \
       transparent"]
;;

(* ── call stack ── *)

(* Blue is where the replay IS, orange is what you are looking at — the same
   pair the heap pane draws, so a box marked orange there and the call that
   allocated it here are visibly one thing. Blue wins when they land on the
   same row: the position is the stronger claim. *)
let stack_row (theme : Theme.t) ~selected ~stripe ~aimed =
  let background =
    match selected, aimed, stripe with
    | true, (true | false), (true | false) -> theme.selection_bg
    (* as strong a wash as the blue one. Sunk 78% toward the background it
       came out at #38281e — the same weight as the zebra stripe, which is
       what a row wears for being ODD. A selection has to outrank that. *)
    | false, true, (true | false) ->
      Theme.mix theme.accent theme.bg ~amount:0.45
    | false, false, true -> theme.stripe_bg
    | false, false, false -> "transparent"
  in
  let border =
    match selected, aimed with
    | true, (true | false) -> theme.selection_border
    | false, true -> theme.accent
    | false, false -> "transparent"
  in
  style
    [%string
      "display:block;white-space:pre-wrap;cursor:pointer;padding:0 10px 0 \
       12px;border-left:3px solid %{border};background:%{background}"]
;;

let stack_glyph (theme : Theme.t) =
  style [%string "color:%{theme.dim};cursor:pointer"]
;;

let stack_hidden (theme : Theme.t) =
  style [%string "color:%{theme.faint};font-style:italic"]
;;

let stack_registered (theme : Theme.t) = style [%string "color:%{theme.dim}"]

(* ── source ── *)

(* [aimed] is the orange reading of the same line: the source pane follows
   whatever the orange mark points at, and the wash says which of the two
   selections put it there — blue for where the replay is, orange for what
   you clicked on in the heap. *)
let source_line (theme : Theme.t) ~active ~aimed =
  let background, border =
    match active, aimed with
    | true, false -> theme.selection_bg, theme.selection_border
    | true, true ->
      Theme.mix theme.accent theme.bg ~amount:0.45, theme.accent
    | false, (true | false) -> "transparent", "transparent"
  in
  style
    [%string
      "display:flex;gap:0;align-items:flex-start;border-left:3px solid \
       %{border};background:%{background}"]
;;

let source_fold_gutter (theme : Theme.t) ~foldable =
  let cursor =
    match foldable with true -> "cursor:pointer;" | false -> ""
  in
  style
    [%string
      "flex:none;width:14px;text-align:center;color:%{theme.ghost};font-size:11px;padding-top:1px;%{cursor}user-select:none"]
;;

let source_number (theme : Theme.t) ~active =
  let color =
    match active with true -> theme.selection_text | false -> theme.ghost
  in
  style
    [%string
      "flex:none;width:38px;text-align:right;padding-right:10px;color:%{color};font-size:12.5px"]
;;

let source_code =
  style
    "flex:1;min-width:0;white-space:pre-wrap;font-size:12.5px;padding-right:10px"
;;

let source_folded_marker (theme : Theme.t) =
  style
    [%string
      "color:%{theme.faint};font-style:italic;font-size:12px;cursor:pointer"]
;;

(* ── heap ── *)

let heap_pane (theme : Theme.t) =
  style
    [%string
      "position:relative;display:grid;grid-template-rows:auto 1fr \
       auto;min-height:0;min-width:0;border:1px solid \
       %{theme.accent};border-top:none;border-right:none"]
;;

let heap_header (theme : Theme.t) =
  style
    [%string
      "display:flex;justify-content:space-between;align-items:baseline;padding:7px \
       14px 6px;color:%{theme.dim}"]
;;

let heap_body = style "position:relative;min-height:0;overflow:hidden"

(* the pane header's two readings of the same heap, side by side with the
   title: one is always chosen, so they are tabs rather than a toggle *)
let heap_title_group = style "display:flex;align-items:baseline;gap:14px"
let heap_tabs = style "display:flex;align-items:baseline;gap:2px"

let heap_tab (theme : Theme.t) ~selected =
  let color, border =
    match selected with
    | true -> theme.accent, theme.accent
    | false -> theme.faint, "transparent"
  in
  style
    [%string
      "padding:0 7px 2px;color:%{color};border-bottom:1px solid \
       %{border};cursor:pointer;user-select:none;letter-spacing:.08em;font-size:12px"]
;;

(* ── heap outline ── *)

(* over the canvas rather than instead of it: the widget owns the pane's
   keyboard, so it stays mounted (and idle, since nothing marks it dirty)
   under an opaque panel *)
let outline_panel (theme : Theme.t) =
  style
    [%string
      "position:absolute;inset:0;background:%{theme.bg};overflow:auto;padding:4px \
       0 0;scrollbar-width:thin;scrollbar-color:%{theme.separator} \
       transparent"]
;;

let outline_row (theme : Theme.t) ~selected ~current ~lead =
  let background =
    match selected with true -> theme.selection_bg | false -> "transparent"
  in
  let border =
    match selected, current with
    | true, (true | false) -> theme.selection_border
    | false, true -> theme.accent
    | false, false -> "transparent"
  in
  (* the breathing room between top-level structures, and never inside one *)
  let lead = match lead with true -> "8px" | false -> "0" in
  style
    [%string
      "display:flex;align-items:baseline;gap:8px;padding:0 12px 0 \
       10px;margin-top:%{lead};border-left:3px solid \
       %{border};background:%{background};cursor:pointer"]
;;

let outline_line = style "flex:1;min-width:0;white-space:pre-wrap"

let outline_address (theme : Theme.t) =
  style [%string "flex:none;color:%{theme.dim};font-size:12px"]
;;

let outline_glyph color =
  style [%string "color:%{color};cursor:pointer;user-select:none"]
;;

let outline_empty (theme : Theme.t) =
  style [%string "padding:10px 16px;color:%{theme.faint}"]
;;

(* what the colors mean, in the colors themselves — the outline speaks four
   text registers plus a fade, which is past what a reader carries in from
   other tools *)
let outline_legend (theme : Theme.t) =
  style
    [%string
      "position:sticky;bottom:0;display:flex;justify-content:flex-end;gap:14px;padding:5px \
       14px 6px;margin-top:8px;background:%{theme.bg};border-top:1px solid \
       %{theme.border};font-size:11.5px"]
;;

let hud (theme : Theme.t) =
  style
    [%string
      "position:absolute;left:14px;bottom:12px;display:flex;align-items:center;gap:14px;padding:5px \
       11px;background:%{theme.hud_bg};border:1px solid \
       %{theme.panel_border};color:%{theme.dim};font-size:11.5px;pointer-events:none;white-space:nowrap;backdrop-filter:blur(3px)"]
;;

let hud_zoom (theme : Theme.t) = style [%string "color:%{theme.text}"]
let hud_model (theme : Theme.t) = style [%string "color:%{theme.accent}"]

(* directly over the canvas's minimap, which stands 118px tall on a 14px
   margin: the two read as one cluster of view controls in the corner *)
let columns_panel (theme : Theme.t) =
  style
    [%string
      "position:absolute;right:14px;bottom:146px;display:flex;align-items:center;gap:9px;padding:5px \
       11px;background:%{theme.hud_bg};border:1px solid \
       %{theme.panel_border};color:%{theme.dim};font-size:11.5px;white-space:nowrap;backdrop-filter:blur(3px)"]
;;

let columns_slider (theme : Theme.t) =
  style
    [%string
      "width:104px;height:12px;margin:0;accent-color:%{theme.accent};background:transparent;cursor:pointer"]
;;

let columns_label (theme : Theme.t) =
  style [%string "color:%{theme.text};min-width:58px"]
;;

let filter_overlay (theme : Theme.t) =
  style
    [%string
      "position:absolute;left:14px;top:12px;display:flex;align-items:center;gap:8px;padding:5px \
       10px;background:%{theme.overlay_bg};border:1px solid \
       %{theme.accent};min-width:280px"]
;;

let filter_slash (theme : Theme.t) = style [%string "color:%{theme.accent}"]

let filter_input (theme : Theme.t) =
  style
    [%string
      "flex:1;color:%{theme.bright};font:inherit;background:transparent;border:0;outline:0"]
;;

let filter_meta (theme : Theme.t) =
  style [%string "color:%{theme.faint};font-size:11.5px"]
;;

(* ── flame drawer ── *)

let flame_drawer (theme : Theme.t) ~open_ =
  let height = match open_ with true -> "180px" | false -> "auto" in
  style
    [%string
      "position:relative;height:%{height};border-top:1px solid \
       %{theme.border};display:grid;grid-template-rows:auto \
       1fr;min-height:0"]
;;

let flame_body (theme : Theme.t) =
  style
    [%string
      "position:relative;overflow-y:auto;min-height:0;scrollbar-width:thin;scrollbar-color:%{theme.separator} \
       transparent"]
;;

let flame_rows ~height = style [%string "position:relative;height:%{height}"]

let flame_bar ~x ~width ~bottom ~height ~fill ~ink ~lit ~deepest =
  let weight = match deepest with true -> "700" | false -> "400" in
  let mark =
    match lit with
    | true -> [%string "border-left:2px solid %{ink};"]
    | false -> ""
  in
  style
    [%string
      "position:absolute;left:%{x}px;width:%{width}px;bottom:%{bottom}px;height:%{height}px;background:%{fill};color:%{ink};%{mark}display:flex;align-items:center;font-size:12px;font-weight:%{weight};overflow:hidden;white-space:nowrap;text-overflow:clip;padding:0 \
       4px;cursor:pointer;box-sizing:border-box"]
;;

let flame_pool (theme : Theme.t) ~x ~width ~bottom ~height =
  style
    [%string
      "position:absolute;left:%{x}px;width:%{width}px;bottom:%{bottom}px;height:%{height}px;background:transparent;color:%{theme.faint};display:flex;align-items:center;font-size:12px;padding:0 \
       4px;white-space:nowrap;box-sizing:border-box"]
;;

(* ── session bar ── *)

let session (theme : Theme.t) =
  style
    [%string
      "display:flex;justify-content:space-between;align-items:center;padding:5px \
       16px;background:%{theme.strip_bg};border-top:1px solid \
       %{theme.border};color:%{theme.dim};font-size:12.5px"]
;;

let session_group = style "display:flex;align-items:center;gap:10px"
let session_right = style "display:flex;align-items:center;gap:8px"
let session_dot (theme : Theme.t) = style [%string "color:%{theme.gold}"]
let session_app (theme : Theme.t) = style [%string "color:%{theme.text}"]

let session_sep (theme : Theme.t) =
  style [%string "color:%{theme.separator}"]
;;

let session_faint (theme : Theme.t) = style [%string "color:%{theme.faint}"]
let legend_cells = style "display:flex;gap:0;height:13px"
let legend_cell color = style [%string "width:17px;background:%{color}"]
let color color = style [%string "color:%{color}"]

let colored ?(italic = false) ?(bold = false) color =
  let italic =
    match italic with true -> "font-style:italic;" | false -> ""
  in
  let bold = match bold with true -> "font-weight:700;" | false -> "" in
  style [%string "color:%{color};%{italic}%{bold}"]
;;
