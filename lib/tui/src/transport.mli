(** The transport strip across the top: tick timeline and the controls.

    Two rows — the per-step tick bar (past dimmed to the same hue, current in
    the highlight blue, future hairline; clickable to jump) over the
    controls: right-aligned chips, each naming its key ([◂ back], [step ▸],
    [[space] play], [q quit]) — the row is simultaneously the buttons and the
    entire key legend, and every chip is clickable. *)

open! Core
module View := Bonsai_term.View

module Button : sig
  type t =
    | Back
    | Step
    | Play
    | Quit
  [@@deriving sexp_of, equal]
end

(** Two rows tall. [step] is 0-based. *)
val view : width:int -> step:int -> total:int -> playing:bool -> View.t

(** Which step a click at column [x] of the tick row jumps to. *)
val step_at : width:int -> total:int -> x:int -> int option

(** Which chip a click at column [x] of the controls row hits — the same
    layout math [view] draws with. [playing] matters: the play chip's label
    (and so every extent) changes with it. *)
val control_at : width:int -> playing:bool -> x:int -> Button.t option
