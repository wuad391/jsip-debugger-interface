open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

let scroll_offset ~count ~height ~selected =
  match count <= height with
  | true -> 0
  | false -> Int.min (Int.max 0 (selected - height + 1)) (count - height)
;;

let row ~width ~index ~is_selected (call : Call.t) =
  let indent = String.make (2 * index) ' ' in
  let fn = Function_info.display call.info.function_info in
  let args =
    List.map call.info.arguments ~f:Argument.display
    |> String.concat ~sep:" "
  in
  let location = Location.display call.info.location in
  let bar =
    match is_selected with
    | true -> View.text ~attrs:(Theme.fg' Theme.accent) "▎"
    | false -> View.text " "
  in
  let fn_color =
    match is_selected with true -> Theme.accent_deep | false -> Theme.text
  in
  let left =
    View.hcat
      [ bar
      ; View.text indent
      ; View.text ~attrs:[ Theme.fg fn_color; Attr.bold ] fn
      ; View.text " "
      ; View.text ~attrs:(Theme.fg' Theme.secondary) args
      ]
  in
  let right = View.text ~attrs:(Theme.fg' Theme.faint) location in
  let gap = max 1 (width - View.width left - View.width right) in
  let line =
    Panel.fit
      (View.hcat
         [ left; View.transparent_rectangle ~width:gap ~height:1; right ])
      ~width
      ~height:1
  in
  match is_selected with
  | true -> View.with_colors' ~fill_backdrop:true ~bg:Theme.accent_bg line
  | false -> line
;;

let view ~width ~height ~frames ~selected =
  let inner_width = Panel.inner_width ~width in
  let inner_height = height - 2 in
  let count = List.length frames in
  let offset = scroll_offset ~count ~height:inner_height ~selected in
  let rows =
    List.mapi frames ~f:(fun index call ->
      row ~width:inner_width ~index ~is_selected:(index = selected) call)
    |> fun rows -> List.drop rows offset
  in
  Panel.view
    ~title:"call stack"
    ~meta:[%string "%{count#Int} live"]
    ~width
    ~height
    (View.vcat rows)
;;

let frame_at ~height ~frames ~selected ~row =
  let inner_height = height - 2 in
  let offset = scroll_offset ~count:frames ~height:inner_height ~selected in
  let index = row + offset in
  match index >= 0 && index < frames with
  | true -> Some index
  | false -> None
;;
