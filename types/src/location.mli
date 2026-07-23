(** The location of a function call

    Used for tracing the relationship between calls and ordering.

    We store the component's [file path] [line] and [character range] *)

open! Core

module T : sig
  type t
end

include T
