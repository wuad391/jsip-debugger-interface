(** An argument inputted into a function called in the program

    Used for tracing what arguments are inserted and manipulated over time

    We store the component's label type, label if it exists and its expression *)

open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]

include Comparable.S with type t := t
include Hashable.S with type t := t