(** The call-stack pane: the whole run's calls, with the live chain lit.

    Every event in the dump gets a row — the callee and its arguments,
    indented by call depth, so the run's shape is visible at once. Where a
    call was written is not repeated here: the source pane below already has
    that line highlighted. Rows on the current step's live chain render
    bright; the selected frame gets the accent bar and wash; everything else
    (already returned, or not reached yet) is dimmed and clicking it jumps
    the replay there. Long argument lists wrap onto continuation lines, and
    the pane scrolls to keep the selection centered. Calls with descendants
    carry a [▾]/[▸] fold glyph; folding hides their whole range (the depth
    bookkeeping's event span) behind a [⋯ n] count without touching any other
    pane.

    Exchange-scale dumps also repeat themselves: thousands of identical leaf
    calls in a row while a book fills. A run of at least four visible leaves
    repeating one function at one depth collapses to a single [fn args ⋯ ×N]
    row whose glyph expands it; a run holding the selection or a live frame
    never collapses, so the rows the eye is following stay individually
    visible. *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** What a click on a row means: select a live frame, jump the replay to a
    dimmed call's step, fold a call's descendants away, or expand/collapse a
    run of repeated calls. *)
module Target : sig
  type t =
    | Frame of int (** index into the live chain *)
    | Step of int (** event index to jump to *)
    | Toggle of int (** fold/unfold this call's range *)
    | Expand of int (** expand/collapse the repeat run headed here *)
  [@@deriving sexp_of, equal]
end

(** [calls] is every event's call in step order; [heat] each call's share of
    the perf profile's sampled compute, same indexing — the share becomes the
    callee name's text color on {!Theme.heat_ramp} ([None] = no data for that
    call, which keeps its ordinary state color; an all-[None] array, e.g. no
    [-perf-file], changes nothing); [live] the step indices of the current
    stack, outermost first; [selected] an index into [live]; [cursor] the
    call the keyboard is aiming at, washed orange over whatever the
    selection's blue is doing, so you can see where [Enter] would go without
    losing where you are; [expanded] the repeat runs (keyed by head index)
    the user has opened back up. *)
val view
  :  width:int
  -> height:int
  -> calls:Call.t array
  -> heat:float option array
  -> live:int list
  -> selected:int
  -> folds:Int.Set.t
  -> cursor:int option
  -> expanded:Int.Set.t
  -> View.t

(** Where [w]/[s] land from the cursor (or, failing that, the selected
    frame): the previous or next call a fold has not tucked away — a
    collapsed run counts once, at its head. [None] at either end — the cursor
    stays put. *)
val move_cursor
  :  calls:Call.t array
  -> live:int list
  -> selected:int
  -> folds:Int.Set.t
  -> cursor:int option
  -> expanded:Int.Set.t
  -> direction:[ `Up | `Down ]
  -> int option

(** What committing a call means — the same choice a click on its row makes:
    a live one selects that frame, any other jumps the replay to it. *)
val target_of : live:int list -> int -> Target.t

(** The head of the repeat run [index] belongs to, if it belongs to one —
    where [h] on any member lands its expand/collapse. *)
val run_head
  :  calls:Call.t array
  -> folds:Int.Set.t
  -> live:int list
  -> selected:int
  -> int
  -> int option

(** The row a click on pane-body position [(x, row)] lands on, mirroring
    [view]'s wrapping and scrolling; a fold glyph's cell yields [Toggle], a
    run glyph's [Expand], anywhere else the row's select/jump target. *)
val target_at
  :  width:int
  -> height:int
  -> calls:Call.t array
  -> heat:float option array
  -> live:int list
  -> selected:int
  -> folds:Int.Set.t
  -> cursor:int option
  -> expanded:Int.Set.t
  -> x:int
  -> row:int
  -> Target.t option
