(** The static type an event states for its walked root.

    The instrumentation prints it off the typedtree at the call site, so the
    interface never parses OCaml type syntax. The wire spells it

    {[
      ty ((printed "int M.t") (params ((key string) (data int))))
    ]}

    [printed] is the whole type as inferred (aliases kept); [params] labels
    the parameter types by role — [key]/[data] for maps and hashtables, [elt]
    for sets and queues. A role the compiler could not resolve (say the
    structure's module is a functor parameter) is simply absent.

    {!Dump_wire} reads it off the event wrapper; {!Call.Info} carries it and
    replay keeps each structure's latest one for the heap pane's headers. *)

open! Core

type t =
  { printed : string
  ; params : (string * string) list
  }
[@@deriving sexp, compare, equal]

(** What a card shows after the structure's kind: [⟨string ⇒ int⟩] when key
    and data are known, [⟨int⟩] for a lone element type, and [⟨printed⟩] when
    no role resolved. *)
val display : t -> string
