(** The transport strip across the top: tick timeline and the controls.

    A per-step tick bar running the full width of the screen (past dimmed to
    the same hue, current in the highlight blue, future hairline; clickable
    to jump) over the controls: right-aligned chips, each naming its key
    ([◂ back], [step ▸], [[space] play], [↑↓ node], [⏎ diagram], [h fold],
    [z accordion], [/ filter], [q quit]) — the row is simultaneously the
    buttons and the key legend, and every chip is clickable. The chips that
    name a mode (play, accordion, diagram) light up while it is on. *)

open! Core
module View := Bonsai_term.View

module Button : sig
  type t =
    | Back
    | Step
    | Play
    | Node (** aims the heap outline's cursor at the next row *)
    | Diagram
    (** pops the row's structure out as the diagram it physically is, and
        dismisses it again *)
    | Fold
    | Accordion
    | Filter
    | Quit
  [@@deriving sexp_of, equal]
end

(** [Layout.strip_height] rows tall — the bar and the controls, back to back,
    on the pane surface so the strip reads as one piece with them. [step] is
    0-based. *)
val view
  :  width:int
  -> step:int
  -> total:int
  -> playing:bool
  -> accordion:bool
  -> diagram:bool
  -> View.t

(** Which step a click at column [x] of the tick row jumps to. *)
val step_at : width:int -> total:int -> x:int -> int option

(** Which chip a click at column [x] of the controls row hits — the same
    layout math [view] draws with. [playing] matters: the play chip's label
    (and so every extent) changes with it. *)
val control_at : width:int -> playing:bool -> x:int -> Button.t option
