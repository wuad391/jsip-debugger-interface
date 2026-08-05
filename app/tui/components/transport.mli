(** The transport strip across the top: tick timeline and the controls.

    A per-step tick bar running the full width of the screen (past dimmed to
    the same hue, current in the highlight blue, future hairline; clickable
    to jump) over the controls: right-aligned chips, each naming its key
    ([◂ back], [step ▸], [[space] play], [h fold], [z accordion], [/ filter],
    [q quit]) — the row is simultaneously the buttons and the key legend, and
    every chip is clickable. The chips that name a mode (play, accordion)
    light up while it is on. *)

open! Core
module View := Bonsai_term.View

module Button : sig
  type t =
    | Back
    | Step
    | Play
    | Fold
    | Accordion
    | Filter
    | Close (** leave the heap's detail view, back to the overview *)
    | Quit
  [@@deriving sexp_of, equal]
end

(** [Layout.strip_height] rows tall — the bar and the controls, back to back,
    on the pane surface so the strip reads as one piece with them. [step] is
    0-based. [density] is per-step activity in [0, 1] (the app derives it
    from each step's fresh allocations against the run's busiest step); past
    and future cells brighten with the activity they cover, so busy phases
    read straight off the bar. [detail] adds the [esc close] chip while a
    structure is open full-pane in the heap. *)
val view
  :  width:int
  -> step:int
  -> total:int
  -> density:float array
  -> playing:bool
  -> accordion:bool
  -> detail:bool
  -> View.t

(** Which step a click at column [x] of the tick row jumps to. *)
val step_at : width:int -> total:int -> x:int -> int option

(** Which chip a click at column [x] of the controls row hits — the same
    layout math [view] draws with. [playing] and [detail] matter: the play
    chip's label and the close chip's presence change every extent. *)
val control_at
  :  width:int
  -> playing:bool
  -> detail:bool
  -> x:int
  -> Button.t option
