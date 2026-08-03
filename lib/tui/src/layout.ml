open! Core
module Dimensions = Bonsai_term.Dimensions
module Position = Bonsai_term.Position
module Region = Bonsai_term.Region

type t =
  { ticks : Region.t
  ; controls : Region.t
  ; top_divider : Region.t
  ; stack : Region.t
  ; source : Region.t
  ; heap : Region.t
  ; column_divider : Region.t
  ; row_divider : Region.t
  ; bottom_divider : Region.t
  ; session : Region.t
  }

let tick_height = 1

(* rows of the transport strip: the bar, then the controls, then the divider
   that closes the strip off — no blank rows between them. The bar is a
   half-height glyph hugging the top of its row, so it already leaves air
   underneath without a whole row spent on it. *)
let strip_height = tick_height + 1

let compute ({ height; width } : Dimensions.t) =
  let controls_y = tick_height in
  let top_divider_y = strip_height in
  let main_y = top_divider_y + 1 in
  (* two rows go below the panes: the session bar and the rule fencing it off
     from them *)
  let main_height = max 6 (height - main_y - 2) in
  (* the left column and the heap split the width; the stack and source split
     the left column; a divider line sits along every seam *)
  let left_width = max 34 (width * 50 / 100) in
  let pane_width = max 1 (left_width - 1) in
  let heap_width = max 20 (width - left_width) in
  let stack_height = max 4 (main_height * 55 / 100) in
  let row_divider_y = main_y + stack_height in
  let source_y = row_divider_y + 1 in
  let source_height = max 3 (main_y + main_height - source_y) in
  { ticks = { x = 0; y = 0; width; height = tick_height }
  ; controls = { x = 0; y = controls_y; width; height = 1 }
  ; top_divider = { x = 0; y = top_divider_y; width; height = 1 }
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
  ; row_divider =
      { x = 0; y = row_divider_y; width = pane_width; height = 1 }
  ; bottom_divider = { x = 0; y = height - 2; width; height = 1 }
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
