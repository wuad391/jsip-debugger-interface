open! Core

type t = { call_order : Call.t Array.t }

val create : parsed_info:Call.Info.t Queue.t -> t
