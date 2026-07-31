(** Where each pane sits for a given terminal size.

    The mockup's grid, in cells: session bar over [stack | heap] and
    [source | heap] columns, transport strip along the bottom. Computed in
    one place so drawing and mouse hit-testing can never disagree. *)

open! Core
module Dimensions := Bonsai_term.Dimensions
module Position := Bonsai_term.Position
module Region := Bonsai_term.Region

type t =
  { top_bar : Region.t
  ; stack : Region.t
  ; source : Region.t
  ; heap : Region.t
  ; ticks : Region.t
  ; controls : Region.t
  }

val compute : Dimensions.t -> t

(** A screen position translated into a pane's inner coordinates (excluding
    the {!Panel} border), or [None] when it falls outside the body. *)
val inner_position : Region.t -> Position.t -> Position.t option
