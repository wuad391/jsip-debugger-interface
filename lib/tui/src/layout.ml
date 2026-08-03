open! Core
module Dimensions = Bonsai_term.Dimensions
module Position = Bonsai_term.Position
module Region = Bonsai_term.Region

type t =
  { ticks : Region.t
  ; controls : Region.t
  ; stack : Region.t
  ; source : Region.t
  ; heap : Region.t
  ; column_divider : Region.t
  ; session : Region.t
  }

let tick_height = 1

(* rows of the top strip: the bar, a space under it, the controls, then the
   gap before the panes *)
let tick_gap = 1
let controls_gap = 2
let strip_height = tick_height + tick_gap + 1 + controls_gap

let compute ({ height; width } : Dimensions.t) =
  let controls_y = tick_height + tick_gap in
  let main_y = strip_height in
  let main_height = max 6 (height - main_y - 1) in
  (* the left column and the heap split the width, with a divider column
     between them; the stack and source split the left column, separated by a
     blank row — the pane titles already say where one ends *)
  let left_width = max 34 (width * 50 / 100) in
  let pane_width = max 1 (left_width - 1) in
  let heap_width = max 20 (width - left_width) in
  let stack_height = max 4 (main_height * 55 / 100) in
  let source_y = main_y + stack_height + 1 in
  let source_height = max 3 (main_y + main_height - source_y) in
  { ticks = { x = 0; y = 0; width; height = tick_height }
  ; controls = { x = 0; y = controls_y; width; height = 1 }
  ; stack = { x = 0; y = main_y; width = pane_width; height = stack_height }
  ; source =
      { x = 0; y = source_y; width = pane_width; height = source_height }
  ; heap =
      { x = left_width
      ; y = main_y
      ; width = heap_width
      ; height = main_height
      }
  ; column_divider =
      { x = pane_width; y = main_y; width = 1; height = main_height }
  ; session = { x = 0; y = height - 1; width; height = 1 }
  }
;;

(* position relative to a pane's body — under the title row, inside the
   column of padding — if it lands there at all *)
let inner_position (region : Region.t) (position : Position.t) =
  match Region.contains region position with
  | false -> None
  | true ->
    let x = position.x - region.x - 1 in
    let y = position.y - region.y - Panel.header_height in
    (match
       x >= 0
       && y >= 0
       && x < Panel.inner_width ~width:region.width
       && y < region.height - Panel.header_height
     with
     | true -> Some ({ x; y } : Position.t)
     | false -> None)
;;
