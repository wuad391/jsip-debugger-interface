(** A function that has been called in the program

    Used for tracking function calls and associating calls with their related
    code file.

    We use a variant [Function] type with either a unique string name or the
    function's whole body *)

open! Core

type t =
  | Function_name of string
  | Unnamed of string
[@@deriving sexp, bin_io, compare, equal, hash]

(** What the interface prints for the callee: the name of a [Function_name],
    or the source text an [Unnamed] carries. *)
val display : t -> string
