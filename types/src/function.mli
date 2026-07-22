(** A function that has been called in the program

    Used for tracking function calls and associating calls with 
    their related code file.

    We use a single [Function] type with a unique string name. *)

open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]

include Comparable.S with type t := t
include Hashable.S with type t := t