(** Loads the source files the dump's locations point into.

    The dump records locations by path (see {!Jsip_types.Location}); the
    interface resolves each path against its source root and loads it here
    once, up front. A missing file is an error the caller renders as a
    placeholder pane, not a crash. *)

open! Core
open Jsip_types

val load : string -> Source_file.t Or_error.t
