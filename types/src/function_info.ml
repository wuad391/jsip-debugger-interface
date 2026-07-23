open! Core

type t =
  | Function_name of string
  | Unnamed of string
[@@deriving sexp, bin_io, compare, equal, hash]
