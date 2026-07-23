open! Core

type t = { call_order : Call.t Array.t }

let create ~size = { call_order = Array.create ~len:size Call.empty }
