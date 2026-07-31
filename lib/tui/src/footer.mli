(** The transport strip along the bottom: tick timeline, buttons, status.

    Three rows — a rule, the mockup's per-step tick bar (past gold-washed,
    current accent, future hairline; clickable to jump), and the controls
    row: back/step/play chips with the key hints right-aligned. *)

open! Core
module View := Bonsai_term.View

module Button : sig
  type t =
    | Back
    | Step
    | Play
  [@@deriving sexp_of, equal]
end

(** Three rows tall. [step] is 0-based. *)
val view : width:int -> step:int -> total:int -> playing:bool -> View.t

(** Which step a click at column [x] of the tick row jumps to. *)
val step_at : width:int -> total:int -> x:int -> int option

(** Which control chip a click at column [x] of the controls row hits. *)
val button_at : x:int -> Button.t option
