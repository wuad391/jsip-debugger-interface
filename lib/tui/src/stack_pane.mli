(** The call-stack pane: the whole run's calls, with the live chain lit.

    Every event in the dump gets a row — the callee, its arguments, and a
    right-aligned [file.ml:line] chip — indented by call depth, so the run's
    shape is visible at once. Rows on the current step's live chain render
    bright; the selected frame gets the accent bar and wash; everything else
    (already returned, or not reached yet) is dimmed and clicking it jumps
    the replay there. Long argument lists wrap onto continuation lines, and
    the pane scrolls to keep the selection centered. *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** What a click on a row means: select a live frame, or jump the replay to a
    dimmed call's step. *)
module Target : sig
  type t =
    | Frame of int (** index into the live chain *)
    | Step of int (** event index to jump to *)
  [@@deriving sexp_of, equal]
end

(** [calls] is every event's call in step order; [live] the step indices of
    the current stack, outermost first; [selected] an index into [live]. *)
val view
  :  width:int
  -> height:int
  -> calls:Call.t array
  -> live:int list
  -> selected:int
  -> View.t

(** The row a click on pane-body row [row] lands on, mirroring [view]'s
    wrapping and scrolling. *)
val target_at
  :  width:int
  -> height:int
  -> calls:Call.t array
  -> live:int list
  -> selected:int
  -> row:int
  -> Target.t option
