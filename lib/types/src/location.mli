(** The location of a function call

    Used for tracing the relationship between calls and ordering.

    We store the component's [file path] [line] and [character range] *)

open! Core

type t [@@deriving sexp]

(* getters *)
val file_path : t -> string
val line_number : t -> int
val char_range : t -> int * int

(* the rest. added this comment so format stops smushing above *)
val create : file_path:string -> line_number:int -> char_range:int * int -> t

(** [file.ml:12] — basename only, the interface's location chip. *)
val display : t -> string
