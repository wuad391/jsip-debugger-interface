(** The call-stack pane's rows, as data: the whole run's calls with the
    live chain lit, folds, repeat runs and registration tags — the TUI
    {!Jsip_tui_components.Stack_pane}'s reading, minus the terminal cells,
    so the web view renders it and expect tests read it. *)

open! Core
open Jsip_types

(** What clicking a row (or its glyph) means — the TUI's contract: a live
    call selects its frame, a dimmed one jumps the replay to its step, a
    fold glyph toggles the call's range, a run glyph expands/collapses the
    repeat run headed there. *)
module Target : sig
  type t =
    | Frame of int (** index into the live chain *)
    | Step of int
    | Toggle of int
    | Expand of int
  [@@deriving sexp_of, equal]
end

module State : sig
  type t =
    | Selected
    | Live
    | Dimmed
  [@@deriving sexp_of, equal]
end

module Glyph : sig
  type t =
    | Blank
    | Fold of bool (** the argument is [folded] *)
    | Run of bool (** the argument is [collapsed] *)
  [@@deriving sexp_of, equal]
end

module Hidden : sig
  type t =
    | Nothing
    | Descendants of int (** the [⋯ n] a fold tucked away *)
    | Repeats of int (** the [⋯ ×N] a collapsed run stands for *)
  [@@deriving sexp_of, equal]
end

module Row : sig
  type t =
    { step : int
    ; depth : int (** call nesting depth, 1-based — the indent *)
    ; state : State.t
    ; heat : float option
    (** the callee name's compute share — its text color *)
    ; fn : string
    ; args : string
    ; glyph : Glyph.t
    ; hidden : Hidden.t
    ; registered : string option
    (** what this call put into the registry, as the [· name] tag *)
    ; target : Target.t
    ; glyph_target : Target.t option
    }
  [@@deriving sexp_of]
end

(** One row per visible call: folds hide their ranges, a collapsed repeat
    run is one row (its head, wearing the [⋯ ×N]). Argument conventions are
    the TUI's: [calls]/[heat]/[registered] indexed by step, [live] the
    current stack's steps outermost first, [selected] an index into [live],
    [folds] fold heads, [expanded] reopened run heads. *)
val rows
  :  calls:Call.t array
  -> heat:float option array
  -> live:int list
  -> selected:int
  -> folds:Int.Set.t
  -> expanded:Int.Set.t
  -> registered:string option array
  -> Row.t list

(** The head of the repeat run [index] belongs to, if any — where [h] on a
    member lands its expand/collapse. *)
val run_head
  :  calls:Call.t array
  -> folds:Int.Set.t
  -> live:int list
  -> selected:int
  -> int
  -> int option

(** Descendants of call [index] — the calls inside its event range. *)
val descendants : Call.t array -> int -> int

(** What committing a call means — the choice a click makes. *)
val target_of : live:int list -> int -> Target.t
