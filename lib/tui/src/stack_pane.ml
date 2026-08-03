open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Target = struct
  type t =
    | Frame of int
    | Step of int
    | Toggle of int
  [@@deriving sexp_of, equal]
end

module Row = struct
  type t =
    { lines : View.t list
    ; call : int
    ; target : Target.t
    ; glyph_x : int option
    ; height : int
    }
end

(* call [i]'s descendants are exactly the calls inside its event range — the
   depth bookkeeping Call_stack already did *)
let descendants (calls : Call.t array) index =
  let (_ : int), hi = calls.(index).Call.range in
  hi - index
;;

let is_hidden ~folds ~calls index =
  Set.exists folds ~f:(fun folded ->
    let (_ : int), hi = calls.(folded).Call.range in
    folded < index && index <= hi)
;;

(* the whole run, one row per visible call: the live chain bright, everything
   else dimmed; a call with descendants gets a fold glyph, and folding tucks
   its range away behind a [⋯ n] count *)
let rows ~width ~calls ~live ~selected ~folds =
  Array.to_list calls
  |> List.filter_mapi ~f:(fun step (call : Call.t) ->
    match is_hidden ~folds ~calls step with
    | true -> None
    | false ->
      let live_index =
        List.findi live ~f:(fun (_ : int) index -> index = step)
        |> Option.map ~f:fst
      in
      let is_selected =
        match live_index with
        | Some frame -> frame = selected
        | None ->
          (* a fold hiding the selected call lights up in its place *)
          (match List.nth live selected with
           | Some selected_step ->
             let (_ : int), hi = call.range in
             Set.mem folds step
             && step < selected_step
             && selected_step <= hi
           | None -> false)
      in
      let bg =
        match is_selected with
        | true -> Some Theme.highlight_bg
        | false -> None
      in
      let fn_attrs, args_color, loc_color =
        match live_index, is_selected with
        | Some _, true | None, true ->
          ( [ Theme.fg Theme.highlight_deep; Attr.bold ]
          , Theme.secondary
          , Theme.faint )
        | Some _, false ->
          [ Theme.fg Theme.text; Attr.bold ], Theme.secondary, Theme.faint
        | None, false -> [ Theme.fg Theme.ghost ], Theme.ghost, Theme.ghost
      in
      let indent = 2 * (call.info.depth - 1) in
      let folded = Set.mem folds step in
      let foldable = descendants calls step > 0 in
      let glyph =
        match foldable, folded with
        | false, _ -> " "
        | true, true -> "▸"
        | true, false -> "▾"
      in
      let fn = Function_info.display call.info.function_info in
      let args =
        List.map call.info.arguments ~f:Argument.display
        |> String.concat ~sep:" "
      in
      let hidden_note =
        match folded with
        | true -> [ `Hidden, [%string " ⋯ %{descendants calls step#Int}"] ]
        | false -> []
      in
      let location = Location.display call.info.location in
      (* the first line leaves room for the location chip and a gap; the
         continuations only for their own two-space indent *)
      let available = width - 1 - indent - 2 in
      let wrapped =
        Wrap.spans
          ([ `Fn, fn; `Args, [%string " %{args}"] ] @ hidden_note)
          ~first_width:(max 8 (available - String.length location - 2))
          ~width:(max 8 (available - 2))
      in
      let bar =
        match is_selected with
        | true -> View.text ~attrs:(Theme.fg' Theme.highlight) "▎"
        | false -> View.text " "
      in
      let render_span (tag, text) =
        match tag with
        | `Fn -> View.text ~attrs:fn_attrs text
        | `Args -> View.text ~attrs:(Theme.fg' args_color) text
        | `Hidden ->
          View.text ~attrs:[ Theme.fg Theme.muted; Attr.italic ] text
      in
      let glyph_x = 1 + indent in
      let lines =
        List.mapi wrapped ~f:(fun line_index spans ->
          let lead =
            match line_index with
            | 0 ->
              [ bar
              ; View.text (String.make indent ' ')
              ; View.text ~attrs:(Theme.fg' Theme.secondary) glyph
              ; View.text " "
              ]
            | _ -> [ bar; View.text (String.make (indent + 4) ' ') ]
          in
          let left = View.hcat (lead @ List.map spans ~f:render_span) in
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
      Some
        { Row.lines
        ; call = step
        ; target
        ; glyph_x =
            (match foldable with true -> Some glyph_x | false -> None)
        ; height = List.length lines
        })
;;

(* keep the selected call's row centered among the wrapped rows — or the fold
   that hides it *)
let scroll_offset rows ~height ~calls ~live ~selected ~folds =
  let target_step =
    match List.nth live selected with
    | None -> None
    | Some step ->
      (match is_hidden ~folds ~calls step with
       | false -> Some step
       | true ->
         (* the innermost visible fold covering it *)
         Set.filter folds ~f:(fun folded ->
           let (_ : int), hi = calls.(folded).Call.range in
           folded < step && step <= hi)
         |> Set.max_elt)
  in
  let target_start, target_height, total =
    List.fold
      rows
      ~init:(0, 1, 0)
      ~f:(fun (start, target_height, total) (row : Row.t) ->
        match
          match target_step with
          | Some target -> row.call = target
          | None -> false
        with
        | true -> total, row.height, total + row.height
        | false -> start, target_height, total + row.height)
  in
  Int.min
    (Int.max 0 (target_start + (target_height / 2) - (height / 2)))
    (Int.max 0 (total - height))
;;

let view ~width ~height ~calls ~live ~selected ~folds =
  let inner_width = Panel.inner_width ~width in
  let inner_height = height - 2 in
  let rows = rows ~width:inner_width ~calls ~live ~selected ~folds in
  let offset =
    scroll_offset rows ~height:inner_height ~calls ~live ~selected ~folds
  in
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

let target_at ~width ~height ~calls ~live ~selected ~folds ~x ~row =
  let inner_width = Panel.inner_width ~width in
  let inner_height = height - 2 in
  let rows = rows ~width:inner_width ~calls ~live ~selected ~folds in
  let offset =
    scroll_offset rows ~height:inner_height ~calls ~live ~selected ~folds
  in
  let target_line = row + offset in
  let rec find rows ~line =
    match rows with
    | [] -> None
    | ({ Row.height; target; glyph_x; call; lines = _ } : Row.t) :: rest ->
      (match line < height with
       | true ->
         (* only the glyph cell on the row's first line toggles *)
         (match glyph_x, line with
          | Some glyph_x, 0 when x = glyph_x -> Some (Target.Toggle call)
          | _ -> Some target)
       | false -> find rest ~line:(line - height))
  in
  find rows ~line:target_line
;;
