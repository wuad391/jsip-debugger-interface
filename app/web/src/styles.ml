open! Core
open Jsip_web_components
module Vdom = Virtual_dom.Vdom

(* This project does not depend on [ppx_css], so [style=] inside [{%html}]
   is off limits; styles are plain [style] attributes built here, named for
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
      "position:relative;display:flex;gap:1px;height:11px;padding:0 \
       2px;background:%{theme.strip_bg};cursor:pointer"]
;;

let tick cell_color = style [%string "flex:1;background:%{cell_color}"]

let playhead (theme : Theme.t) left =
  style
    [%string
      "position:absolute;top:-1px;bottom:-1px;width:2px;background:%{theme.playhead};box-shadow:0 \
       0 6px rgba(128,132,140,.7);pointer-events:none;left:%{left}"]
;;

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
    [%string "display:grid;grid-template-rows:%{rows};min-height:0;min-width:0"]
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
  let cursor = match clickable with true -> "cursor:pointer;" | false -> "" in
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

let pane_body = style "overflow-y:auto;overflow-x:hidden;padding:0 0 14px"

(* ── call stack ── *)

let stack_row (theme : Theme.t) ~selected ~stripe =
  let background =
    match selected, stripe with
    | true, (true | false) -> theme.selection_bg
    | false, true -> theme.stripe_bg
    | false, false -> "transparent"
  in
  let border =
    match selected with
    | true -> theme.selection_border
    | false -> "transparent"
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

let source_line (theme : Theme.t) ~active =
  let background, border =
    match active with
    | true -> theme.selection_bg, theme.selection_border
    | false -> "transparent", "transparent"
  in
  style
    [%string
      "display:flex;gap:0;align-items:flex-start;border-left:3px solid \
       %{border};background:%{background}"]
;;

let source_fold_gutter (theme : Theme.t) ~foldable =
  let cursor = match foldable with true -> "cursor:pointer;" | false -> "" in
  style
    [%string
      "flex:none;width:14px;text-align:center;color:%{theme.ghost};font-size:11px;padding-top:1px;%{cursor}user-select:none"]
;;

let source_number (theme : Theme.t) ~active =
  let color =
    match active with
    | true -> theme.selection_text
    | false -> theme.ghost
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

let hud (theme : Theme.t) =
  style
    [%string
      "position:absolute;left:14px;bottom:12px;display:flex;align-items:center;gap:14px;padding:5px \
       11px;background:%{theme.hud_bg};border:1px solid \
       %{theme.panel_border};color:%{theme.dim};font-size:11.5px;pointer-events:none;white-space:nowrap;backdrop-filter:blur(3px)"]
;;

let hud_zoom (theme : Theme.t) = style [%string "color:%{theme.text}"]
let hud_model (theme : Theme.t) = style [%string "color:%{theme.accent}"]

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
       %{theme.border};display:grid;grid-template-rows:auto 1fr;min-height:0"]
;;

let flame_body = style "position:relative;overflow-y:auto;min-height:0"
let flame_rows ~height = style [%string "position:relative;height:%{height}"]

let flame_bar ~x ~width ~bottom ~fill ~ink ~lit ~deepest =
  let weight = match deepest with true -> "700" | false -> "400" in
  let mark =
    match lit with
    | true -> [%string "border-left:2px solid %{ink};"]
    | false -> ""
  in
  style
    [%string
      "position:absolute;left:%{x}px;width:%{width}px;bottom:%{bottom}px;height:17px;background:%{fill};color:%{ink};%{mark}font-size:11px;font-weight:%{weight};overflow:hidden;white-space:nowrap;text-overflow:clip;padding:1px \
       3px 0;cursor:pointer;box-sizing:border-box"]
;;

let flame_pool (theme : Theme.t) ~x ~width ~bottom =
  style
    [%string
      "position:absolute;left:%{x}px;width:%{width}px;bottom:%{bottom}px;height:17px;background:transparent;color:%{theme.faint};font-size:11px;padding:1px \
       3px 0;white-space:nowrap;box-sizing:border-box"]
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
  let italic = match italic with true -> "font-style:italic;" | false -> "" in
  let bold = match bold with true -> "font-weight:700;" | false -> "" in
  style [%string "color:%{color};%{italic}%{bold}"]
;;
