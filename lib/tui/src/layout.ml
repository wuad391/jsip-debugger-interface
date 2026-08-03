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
  ; session : Region.t
  }

let compute ({ height; width } : Dimensions.t) =
  (* the transport strip (ticks, controls, rule) across the top; the session
     bar across the bottom *)
  let transport_height = 3 in
  let main_y = transport_height in
  let main_height = max 6 (height - transport_height - 1) in
  (* mockup grid: stack row 1.28fr over source 0.72fr; both take half the
     width, the heap the other half *)
  let left_width = max 34 (width * 50 / 100) in
  let heap_width = max 20 (width - left_width) in
  let stack_height = max 4 (main_height * 55 / 100) in
  let source_height = max 4 (main_height - stack_height) in
  { ticks = { x = 0; y = 0; width; height = 1 }
  ; controls = { x = 0; y = 1; width; height = 1 }
  ; stack = { x = 0; y = main_y; width = left_width; height = stack_height }
  ; source =
      { x = 0
      ; y = main_y + stack_height
      ; width = left_width
      ; height = source_height
      }
  ; heap =
      { x = left_width
      ; y = main_y
      ; width = heap_width
      ; height = main_height
      }
  ; session = { x = 0; y = height - 1; width; height = 1 }
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
