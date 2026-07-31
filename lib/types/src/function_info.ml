open! Core

type t =
  | Function_name of string
  | Unnamed of string
[@@deriving sexp, bin_io, compare, equal, hash]

let display t =
  match t with Function_name name -> name | Unnamed source -> source
;;
