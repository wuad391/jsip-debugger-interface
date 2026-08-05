(** What the source's names mean at one point in the program — the wire's
    answer to "can this structure still be reached by its name?".

    Every event carries the scope of the program point it fired from: each
    name that could reach a tracked structure there, paired with the
    {!Binder.t} it resolves to. {!Registry_entry} says what a structure is
    currently CALLED; this says what that name currently MEANS. A structure
    is still reachable exactly when the two agree — which is what greys a
    shadowed copy out in the heap pane. [Replay.Visibility] in [jsip_replay]
    is where the comparison happens.

    For [let m = M.empty in let m = M.add "a" 1 m in ...], the last event
    carries

    {v
    (binder Map_basic.m_88) (scope ((m Map_basic.m_88)))
    v}

    Three live structures called [m], one binder: the two older ones are
    shadowed. A dump from a compiler predating these fields carries no scope
    at all, which reads as [None] and leaves visibility unknown rather than
    guessed. *)

open! Core

(** Which [let] introduced a name: the compiler's own unique spelling of one
    binding — [Map_basic.m_88] is the identifier [m], with the stamp that
    separates it from every other [m], in the unit that bound it.

    Two observations share a binder exactly when they were bound by the same
    [let], so this is what tells a structure apart from the one that shadowed
    it. The name alone cannot: after [let m = M.add "a" 1 m] both versions
    are called [m], and only one of them still answers to it. *)
module Binder : sig
  type t = string [@@deriving sexp, compare, equal]

  (** The compiler's spelling, for diagnostics — it is not a source path and
      nothing resolves it. *)
  val display : t -> string
end

type t = Binder.t String.Map.t [@@deriving sexp]

val empty : t

(** What [name] means here, if anything is bound under it. *)
val binder : t -> name:string -> Binder.t option

(** [nest ~outer ~inner] is [outer] with [inner]'s names layered over it —
    the scope seen from a call stack, where the innermost frame wins. A
    structure the caller bound stays reachable while a helper runs, unless
    the helper bound that name itself. *)
val nest : outer:t -> inner:t -> t
