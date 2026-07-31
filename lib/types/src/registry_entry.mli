(** One entry of the live weak registry an event carries: a tracked
    structure's stable id, its address at event time, and the latest
    non-empty variable name it was observed under (a later event may rename
    it; a structure bound namelessly has none).

    The wire spells a named entry [(1 0x7f2ce89e q)] and an anonymous one
    [(1 0x7f2ce89e)]; the sexp functions here read and write exactly those
    shapes. Entries appear when a structure is first tracked and disappear
    once the GC has collected it. *)

open! Core

type t =
  { id : int
  ; address : Snapshot.Address.t
  ; name : string option
  }
[@@deriving sexp]

(** The structure's display identity: its name, or [#id] when it never had
    one. *)
val display : t -> string
