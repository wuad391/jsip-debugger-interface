open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Target = struct
  type t =
    | Frame of int
    | Step of int
  [@@deriving sexp_of, equal]
end

module Row = struct
  type t =
    { lines : View.t list
    ; target : Target.t
    ; height : int
    }
end

(* the whole run, one row per call: the live chain bright, everything else
   dimmed; long argument lists wrap onto continuation lines *)
let rows ~width ~calls ~live ~selected =
  Array.to_list calls
  |> List.mapi ~f:(fun step (call : Call.t) ->
    let live_index =
      List.findi live ~f:(fun (_ : int) index -> index = step)
      |> Option.map ~f:fst
    in
    let is_selected =
      match live_index with Some frame -> frame = selected | None -> false
    in
    let bg =
      match is_selected with
      | true -> Some Theme.highlight_bg
      | false -> None
    in
    let fn_attrs, args_color, loc_color =
      match live_index, is_selected with
      | Some _, true ->
        ( [ Theme.fg Theme.highlight_deep; Attr.bold ]
        , Theme.secondary
        , Theme.faint )
      | Some _, false ->
        [ Theme.fg Theme.text; Attr.bold ], Theme.secondary, Theme.faint
      | None, _ -> [ Theme.fg Theme.ghost ], Theme.ghost, Theme.ghost
    in
    let indent = 2 * (call.info.depth - 1) in
    let fn = Function_info.display call.info.function_info in
    let args =
      List.map call.info.arguments ~f:Argument.display
      |> String.concat ~sep:" "
    in
    let location = Location.display call.info.location in
    (* the first line leaves room for the location chip and a gap; the
       continuations only for their own two-space indent *)
    let available = width - 1 - indent in
    let wrapped =
      Wrap.spans
        [ `Fn, fn; `Args, [%string " %{args}"] ]
        ~first_width:(max 8 (available - String.length location - 2))
        ~width:(max 8 (available - 2))
    in
    let bar line_index =
      match is_selected, line_index with
      | true, _ -> View.text ~attrs:(Theme.fg' Theme.highlight) "▎"
      | false, _ -> View.text " "
    in
    let render_span (tag, text) =
      match tag with
      | `Fn -> View.text ~attrs:fn_attrs text
      | `Args -> View.text ~attrs:(Theme.fg' args_color) text
    in
    let lines =
      List.mapi wrapped ~f:(fun line_index spans ->
        let continuation = match line_index with 0 -> 0 | _ -> 2 in
        let left =
          View.hcat
            (bar line_index
             :: View.text (String.make (indent + continuation) ' ')
             :: List.map spans ~f:render_span)
        in
        let line =
          match line_index with
          | 0 ->
            let right = View.text ~attrs:(Theme.fg' loc_color) location in
            let gap = max 1 (width - View.width left - View.width right) in
            View.hcat
              [ left
              ; View.transparent_rectangle ~width:gap ~height:1
              ; right
              ]
          | _ -> left
        in
        let line = Panel.fit line ~width ~height:1 in
        match bg with
        | Some bg -> View.with_colors' ~fill_backdrop:true ~bg line
        | None -> line)
    in
    let target =
      match live_index with
      | Some frame -> Target.Frame frame
      | None -> Target.Step step
    in
    { Row.lines; target; height = List.length lines })
;;

(* keep the selected live row centered among the wrapped rows *)
let scroll_offset rows ~height ~live ~selected =
  let target_step = List.nth live selected in
  let target_start, target_height, total =
    List.foldi
      rows
      ~init:(0, 1, 0)
      ~f:(fun step (start, target_height, total) (row : Row.t) ->
        match
          match target_step with
          | Some target -> step = target
          | None -> false
        with
        | true -> total, row.height, total + row.height
        | false -> start, target_height, total + row.height)
  in
  Int.min
    (Int.max 0 (target_start + (target_height / 2) - (height / 2)))
    (Int.max 0 (total - height))
;;

let view ~width ~height ~calls ~live ~selected =
  let inner_width = Panel.inner_width ~width in
  let inner_height = height - 2 in
  let rows = rows ~width:inner_width ~calls ~live ~selected in
  let offset = scroll_offset rows ~height:inner_height ~live ~selected in
  let body =
    List.concat_map rows ~f:(fun (row : Row.t) -> row.lines)
    |> fun lines -> List.drop lines offset
  in
  Panel.view
    ~title:"call stack"
    ~meta:
      [%string
        "%{Array.length calls#Int} calls · %{List.length live#Int} live"]
    ~width
    ~height
    (View.vcat body)
;;

let target_at ~width ~height ~calls ~live ~selected ~row =
  let inner_width = Panel.inner_width ~width in
  let inner_height = height - 2 in
  let rows = rows ~width:inner_width ~calls ~live ~selected in
  let offset = scroll_offset rows ~height:inner_height ~live ~selected in
  let target_line = row + offset in
  let rec find rows ~line =
    match rows with
    | [] -> None
    | ({ Row.height; target; lines = _ } : Row.t) :: rest ->
      (match line < height with
       | true -> Some target
       | false -> find rest ~line:(line - height))
  in
  find rows ~line:target_line
;;
