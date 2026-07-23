(** The location of a function call

    Used for tracing the relationship between calls and ordering.

    We store the component's [file path] [line] and [character range] *)

open! Core

module T : sig
  type t
end

include T

(* getters *)
val file_path : t -> string
val line_number : t -> int
val char_start : t -> int
val char_end : t -> int
val char_range : t -> int * int

(* the rest. added this comment so format stops smushing above *)
val create : file:string -> line:int -> char_range:int * int -> t
