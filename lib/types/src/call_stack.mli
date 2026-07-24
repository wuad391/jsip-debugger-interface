open! Core

type t = { call_order : Call.t Array.t }

val create : size:int -> t
