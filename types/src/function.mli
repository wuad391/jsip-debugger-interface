(** A function that has been called in the program

    Used for tracking function calls and associating calls with their related
    code file.

    We use a variant [Function] type with either a unique string name or the
    function's whole body *)

open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t
