(** Loads the heat profile the pipeline's perf stage writes.

    The visual-debugger shell's [cool_name.sh] samples the unchanged program
    under [perf] and leaves a [heat.sexp] next to the dump; the app's
    [-perf-file] flag points here. Like {!Source_reader} for source files,
    this is the parsing layer's one place that touches the file system for it
    — a missing or garbled file is an error the caller decides how to render,
    not a crash. *)

open! Core
open Jsip_types

val load : string -> Heat_profile.t Or_error.t

(** The same reader over a profile already in memory — the web interface's
    entry point after fetching [heat.sexp] over HTTP. *)
val parse : string -> Heat_profile.t Or_error.t
