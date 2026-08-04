(** Where each pane sits for a given terminal size.

    In cells: the transport strip (the tick bar over the controls) across the
    top, then [stack | heap] and [source | heap] columns, and the session bar
    along the bottom. A single divider line runs along every seam, including
    the two that fence the panes off from the strip above and the session bar
    below; the panes themselves have no borders. Computed in one place so
    drawing and mouse hit-testing can never disagree. *)

open! Core
module Dimensions := Bonsai_term.Dimensions
module Position := Bonsai_term.Position
module Region := Bonsai_term.Region

(** Rows the tick bar occupies — {!Transport} draws that many. *)
val tick_height : int

(** Rows of chrome above the panes: the bar and the controls, back to back.
    {!Transport} fills the whole strip so it reads as one surface. *)
val strip_height : int

type t =
  { ticks : Region.t
  ; controls : Region.t
  ; top_divider : Region.t (** between the transport strip and the panes *)
  ; stack : Region.t
  ; source : Region.t
  ; heap : Region.t
  ; column_divider : Region.t (** between the left column and the heap *)
  ; row_divider : Region.t (** between the stack and the source *)
  ; bottom_divider : Region.t (** between the panes and the session bar *)
  ; session : Region.t
  }

val compute : Dimensions.t -> t

(** A screen position translated into a pane's body coordinates (below the
    title row, inside the padding), or [None] when it falls outside. *)
val inner_position : Region.t -> Position.t -> Position.t option
