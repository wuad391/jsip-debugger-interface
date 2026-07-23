(** A function that has been called in the program

    Used for tracking function calls and associating calls with their related
    code file.

    We use a variant [Function] type with either a unique string name or the
    function's whole body *)

open! Core

module T : sig
  type t =
    | Function_name of string
    | Unnamed of string
  [@@deriving sexp, bin_io, compare, equal, hash]
end

include T
include Comparable.S with type t := t
include Hashable.S with type t := t
