(** Reads a [-visual-replay] dump file into the events it holds.

    Each line is a run of [{}] depth markers followed by one event sexp; this
    tracks the depth and hands the payload to {!Dump_wire}. The result feeds
    {!Jsip_types.Call_stack.create} directly. *)

open! Core
open Jsip_types

(** Every event in the dump at [file_path], in file order. Errors — an
    unreadable file, a malformed event, a dump that never returns to depth 0
    — come back tagged with the line they were found on. *)
val read : string -> Call.Info.t Queue.t Or_error.t
