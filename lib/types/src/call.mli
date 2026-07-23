open! Core

type t =
  { depth : int
  ; function_info : Function_info.t
  ; location : Location.t
  ; arguments : Argument.t list
  ; call_range : int * int
  }

(* the empty, default call to be populated later *)
val empty : t
