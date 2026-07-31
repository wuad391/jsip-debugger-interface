(** One source file the dump's locations point into, held as lines.

    The interface's source pane reads it by the 1-based line numbers that
    {!Location.line_number} carries. Loading from disk lives in the parsing
    layer ([Source_reader]); this is just the loaded shape. *)

open! Core

type t

val of_lines : Line.t list -> t

(** The 1-based [number]th line, or [None] outside [1 .. length t]. *)
val line : t -> number:int -> Line.t option

val length : t -> int
