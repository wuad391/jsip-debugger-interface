(*=open! Core

type t = { source : Line.t Array.t }

val create : ~size:int -> t

val get_line : t -> int -> Line.t*)
