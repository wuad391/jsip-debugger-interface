(** The call-stack pane: the whole run's calls, with the live chain lit.

    Every event in the dump gets a row — the callee, its arguments, and a
    right-aligned [file.ml:line] chip — indented by call depth, so the run's
    shape is visible at once. Rows on the current step's live chain render
    bright; the selected frame gets the accent bar and wash; everything else
    (already returned, or not reached yet) is dimmed and clicking it jumps
    the replay there. Long argument lists wrap onto continuation lines, and
    the pane scrolls to keep the selection centered. Calls with descendants
    carry a [▾]/[▸] fold glyph; folding hides their whole range (the depth
    bookkeeping's event span) behind a [⋯ n] count without touching any other
    pane. *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** What a click on a row means: select a live frame, jump the replay to a
    dimmed call's step, or fold a call's descendants away. *)
module Target : sig
  type t =
    | Frame of int (** index into the live chain *)
    | Step of int (** event index to jump to *)
    | Toggle of int (** fold/unfold this call's range *)
  [@@deriving sexp_of, equal]
end

(** [calls] is every event's call in step order; [live] the step indices of
    the current stack, outermost first; [selected] an index into [live];
    [cursor] the call the keyboard is aiming at, washed orange over whatever
    the selection's blue is doing, so you can see where [Enter] would go
    without losing where you are. *)
val view
  :  width:int
  -> height:int
  -> calls:Call.t array
  -> live:int list
  -> selected:int
  -> folds:Int.Set.t
  -> cursor:int option
  -> View.t

(** Where [w]/[s] land from the cursor (or, failing that, the selected
    frame): the previous or next call a fold has not tucked away. [None] at
    either end — the cursor stays put. *)
val move_cursor
  :  calls:Call.t array
  -> live:int list
  -> selected:int
  -> folds:Int.Set.t
  -> cursor:int option
  -> direction:[ `Up | `Down ]
  -> int option

(** What committing a call means — the same choice a click on its row makes:
    a live one selects that frame, any other jumps the replay to it. *)
val target_of : live:int list -> int -> Target.t

(** The row a click on pane-body position [(x, row)] lands on, mirroring
    [view]'s wrapping and scrolling; the fold glyph's cell yields [Toggle],
    anywhere else the row's select/jump target. *)
val target_at
  :  width:int
  -> height:int
  -> calls:Call.t array
  -> live:int list
  -> selected:int
  -> folds:Int.Set.t
  -> cursor:int option
  -> x:int
  -> row:int
  -> Target.t option
