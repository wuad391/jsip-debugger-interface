(*=open! Core

type t = { source : Line.t Array.t }

let create ~size = { source = Array.create ~len:size Call.empty }
let get_line t i = Array.get t.source (i - 1)*)
