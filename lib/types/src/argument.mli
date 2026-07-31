(** An argument inputted into a function called in the program

    Used for tracing what arguments are inserted and manipulated over time

    We store the component's label type, label if it exists and its
    expression *)

open! Core

type t =
  | No_label of { expression : Function_info.t }
  | Labelled of
      { label : string
      ; expression : Function_info.t
      }
  | Optional of
      { label : string
      ; expression : Function_info.t
      }
[@@deriving sexp, bin_io]

(** The argument as it would read at the call site: bare, [~label:expr], or
    [?label:expr]; the compiler spells an argument the application was
    abstracted over [OMITTED], which displays as [_]. *)
val display : t -> string
