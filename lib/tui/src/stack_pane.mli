(** The call-stack pane: one row per live frame, outermost first.

    Each row shows the callee, its arguments, and a right-aligned
    [file.ml:line] chip, indented by nesting — the mockup's stack panel. The
    selected frame gets the accent bar and background; selection is the
    app's, this module just draws it. Scrolls to keep the selection visible
    when the stack is deeper than the pane. *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** [frames] outermost first, [selected] an index into it. *)
val view
  :  width:int
  -> height:int
  -> frames:Call.t list
  -> selected:int
  -> View.t

(** Which frame a click on pane-body row [row] (0 = first row inside the
    border) lands on, mirroring [view]'s scrolling. [frames] is the count. *)
val frame_at
  :  height:int
  -> frames:int
  -> selected:int
  -> row:int
  -> int option
