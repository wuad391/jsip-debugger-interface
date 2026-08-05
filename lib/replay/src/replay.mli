(** One dump made replayable: the precomputed view of every step.

    Sits between {!Jsip_types.Call_stack} and the interface. The stack gives
    the raw timeline; this module walks it once and keeps, per step,
    everything a renderer asks for while stepping back and forth — the live
    frames, the live structures, and which heap addresses were first seen at
    that step (the interface's "freshly allocated" highlight).

    {[
      let replay = Replay.create call_stack in
      let step = Replay.step_exn replay ~step:3 in
      Replay.description step.call
      (* "M.add \"b\" 2 m — map_smoke.ml:6" *)
    ]} *)

open! Core
open Jsip_types

(** Whether a live structure can still be reached by the name it goes under.

    Alive and reachable are different questions: [let m = M.add "a" 1 m]
    leaves both versions in the registry, both called [m], but only the new
    one answers to that name — and a structure bound inside a call that has
    returned is alive until the GC takes it while being nameable nowhere. The
    heap pane greys out everything but {!In_scope}, so an old copy fades
    while the live one stays lit.

    Computed from the wire's {!Jsip_types.Scope}: [In_scope] when the name
    the registry knows the structure by still resolves to the binding it was
    observed under, [Shadowed] when that name now means a different binding,
    [Out_of_scope] when nothing on the stack binds it any more.

    [Unknown] is the honest gap — a structure never observed under a name, or
    a dump from a compiler that emits no scope. Neither is evidence of being
    unreachable, so both render as though in scope. *)
module Visibility : sig
  type t =
    | In_scope
    | Shadowed
    | Out_of_scope
    | Unknown
  [@@deriving sexp_of, equal]

  (** [In_scope] and [Unknown]. What the pane draws at full strength. *)
  val is_reachable : t -> bool
end

(** One tracked structure alive at a step: a registry entry joined with the
    shape of that structure's most recent walk. A structure stays here, at
    its latest shape, until the registry drops it (the GC collected it);
    [is_current] marks the one this step's event walked. *)
module Structure : sig
  type t =
    { id : int
    ; name : string option
    (** the latest variable name the structure was observed under *)
    ; ty : Type_info.t option
    (** the structure's static type, from its latest event that stated one;
        [None] when every event predates the wire's [ty] field *)
    ; address : Snapshot.Address.t
    ; snapshot : Snapshot.t
    ; is_current : bool
    ; visibility : Visibility.t
    (** whether that name still reaches this structure, here *)
    }

  (** [name], or [#id] for a structure never observed under one. *)
  val display : t -> string

  (** [snapshot]'s root stamped with [address] — the registry re-captures
      every structure's address at every event, so a renderer drawing an
      older walk still shows the root address of record. *)
  val current_root : t -> Snapshot.Node.t
end

(** The dump is a stream of deltas: every node's definition appears once
    under its wire id, and later occurrences are [Snapshot.Block.Id]
    references — a shared block, a cycle, or a whole re-observed structure
    collapsed to a stub. This is the table those references resolve against,
    as of one step. *)
module Nodes : sig
  type t = Snapshot.Node.t Int.Map.t

  (** The node carrying that wire id, if the dump has defined it yet. *)
  val find : t -> int -> Snapshot.Node.t option
end

module Step : sig
  type t =
    { call : Call.t (** the event that fired at this step *)
    ; frames : Call.t list (** the live stack, outermost first *)
    ; structures : Structure.t list
    (** every live tracked structure, in registry order; each one's root is
        the resolved definition, never a stub *)
    ; nodes : Nodes.t
    (** every node the dump has defined up to and including this step *)
    ; new_addresses : Snapshot.Address.Set.t
    (** addresses in this step's snapshot that no earlier snapshot mentioned
        — the nodes this call allocated *)
    }
end

(** [fn args — file.ml:line] for one event. Computed on demand rather than
    stored on every step, because nothing on screen shows it. *)
val description : Call.t -> string

type t

val create : Call_stack.t -> t

(** Number of steps — same as the dump's event count. *)
val length : t -> int

(** Raises if [step] is outside [0 .. length t - 1]. *)
val step_exn : t -> step:int -> Step.t

(** Every file the dump mentions, in order of first appearance — what the
    source pane may need to load. *)
val files : t -> string list
