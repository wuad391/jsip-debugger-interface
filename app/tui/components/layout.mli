(** Where each pane sits for a given terminal size.

    In cells: the transport strip (the tick bar over the controls) across the
    top, then [stack | heap] and [source | flame] columns, and the session
    bar along the bottom. A single divider line runs along every seam,
    including the two that fence the panes off from the strip above and the
    session bar below; the panes themselves have no borders. Computed in one
    place so drawing and mouse hit-testing can never disagree.

    The right column splits the way the left one does. The flame drawer
    ({!Flame_pane}) takes the bottom of it, and [flame_open] says whether it
    is open or shut — shut, it keeps only its title row, which is still a
    region, so it stays on screen to say it is there and to be clicked:

    {v
    ┌──── ticks ──────────────────────────────────┐
    │ controls                                    │
    ├───────────────────┬─────────────────────────┤  flame_open
    │ stack             │ heap                    │  = false
    ├───────────────────┤                         │
    │ source            ├─────────────────────────┤
    │                   │ flame (title row only)  │
    ├───────────────────┴─────────────────────────┤
    │ session                                     │

    ┌──── ticks ──────────────────────────────────┐
    │ controls                                    │
    ├───────────────────┬─────────────────────────┤  flame_open
    │ stack             │ heap                    │  = true
    ├───────────────────┤                         │
    │ source            ├─────────────────────────┤
    │                   │ flame                   │
    │                   │                         │
    ├───────────────────┴─────────────────────────┤
    │ session                                     │
    v}

    Nothing but the seam between them moves: opening the drawer takes rows
    from the heap and gives them back on close, and every other pane keeps
    its box. *)

open! Core
module Dimensions := Bonsai_term.Dimensions
module Position := Bonsai_term.Position
module Region := Bonsai_term.Region

(** Rows the tick bar occupies — {!Transport} draws that many. *)
val tick_height : int

(** Rows of chrome above the panes: the bar and the controls, back to back.
    {!Transport} fills the whole strip so it reads as one surface. *)
val strip_height : int

(** What a shut flame drawer keeps: its title row, and nothing else. *)
val collapsed_flame_height : int

type t =
  { ticks : Region.t
  ; controls : Region.t
  ; top_divider : Region.t (** between the transport strip and the panes *)
  ; stack : Region.t
  ; source : Region.t
  ; heap : Region.t
  ; flame : Region.t
  (** the drawer under the heap; one row tall while it is shut *)
  ; column_divider : Region.t (** between the left column and the right *)
  ; row_divider : Region.t (** between the stack and the source *)
  ; heap_divider : Region.t (** between the heap and the flame drawer *)
  ; bottom_divider : Region.t (** between the panes and the session bar *)
  ; session : Region.t
  }
[@@deriving sexp_of]

val compute : Dimensions.t -> flame_open:bool -> t

(** A screen position translated into a pane's body coordinates (below the
    title row, inside the padding), or [None] when it falls outside — which
    for a pane's own title row is how a click on it is told apart from a
    click on its contents. *)
val inner_position : Region.t -> Position.t -> Position.t option
