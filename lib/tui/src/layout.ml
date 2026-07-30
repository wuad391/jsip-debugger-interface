open! Core
module Dimensions = Bonsai_term.Dimensions
module Position = Bonsai_term.Position
module Region = Bonsai_term.Region

type t =
  { top_bar : Region.t
  ; stack : Region.t
  ; source : Region.t
  ; heap : Region.t
  ; ticks : Region.t
  ; controls : Region.t
  }

let compute ({ height; width } : Dimensions.t) =
  let footer_height = 3 in
  let main_height = max 6 (height - 1 - footer_height) in
  (* mockup grid: left column 1fr, heap 1.32fr; stack row 1.28fr over source
     0.72fr *)
  let left_width = max 34 (width * 43 / 100) in
  let heap_width = max 20 (width - left_width) in
  let stack_height = max 4 (main_height * 55 / 100) in
  let source_height = max 4 (main_height - stack_height) in
  { top_bar = { x = 0; y = 0; width; height = 1 }
  ; stack = { x = 0; y = 1; width = left_width; height = stack_height }
  ; source =
      { x = 0
      ; y = 1 + stack_height
      ; width = left_width
      ; height = source_height
      }
  ; heap =
      { x = left_width; y = 1; width = heap_width; height = main_height }
  ; ticks = { x = 0; y = height - 2; width; height = 1 }
  ; controls = { x = 0; y = height - 1; width; height = 1 }
  }
;;

(* position relative to a pane's inner (borderless) box, if inside it *)
let inner_position (region : Region.t) (position : Position.t) =
  match Region.contains region position with
  | false -> None
  | true ->
    let x = position.x - region.x - 1 in
    let y = position.y - region.y - 1 in
    (match
       x >= 0 && y >= 0 && x < region.width - 2 && y < region.height - 2
     with
     | true -> Some ({ x; y } : Position.t)
     | false -> None)
;;
