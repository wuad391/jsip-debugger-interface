(** One dump made replayable: the precomputed view of every step.

    Sits between {!Jsip_types.Call_stack} and the interface. The stack gives
    the raw timeline; this module walks it once and keeps, per step,
    everything a renderer asks for while stepping back and forth — the live
    frames, which heap addresses were first seen at that step (the
    interface's "freshly allocated" highlight), and the one-line status
    description.

    {[
      let replay = Replay.create call_stack in
      let step = Replay.step_exn replay ~step:3 in
      step.description (* "M.add \"b\" 2 m — map_smoke.ml:6" *)
    ]} *)

open! Core
open Jsip_types

module Step : sig
  type t =
    { call : Call.t (** the event that fired at this step *)
    ; frames : Call.t list (** the live stack, outermost first *)
    ; new_addresses : Snapshot.Address.Set.t
    (** addresses in this step's snapshot that no earlier snapshot mentioned
        — the nodes this call allocated *)
    ; description : string
    (** [fn args — file.ml:line], the footer status line *)
    }
end

type t

val create : Call_stack.t -> t

(** Number of steps — same as the dump's event count. *)
val length : t -> int

(** Raises if [step] is outside [0 .. length t - 1]. *)
val step_exn : t -> step:int -> Step.t

(** Every file the dump mentions, in order of first appearance — what the
    source pane may need to load. *)
val files : t -> string list
