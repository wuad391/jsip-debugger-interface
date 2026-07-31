(** The source pane: the selected frame's file with the mockup's cues.

    Line numbers in a gutter, {!Syntax}-colored code, the active line washed
    in the accent background with a [▎] bar, the caller's line marked [▸]
    when it is in the same file, and the event's [char_range] underlined on
    the active line. Long lines wrap onto continuation rows (blank gutter)
    rather than cropping, and the pane scrolls to keep the active line
    centered. *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** A source file with its highlighting precomputed — do this once per file
    at startup, not per frame. *)
module Loaded : sig
  type t =
    { file : Source_file.t
    ; spans : (Syntax.Token.t * string) list Array.t
    }

  val of_source_file : Source_file.t -> t
end

(** [source] is an [Or_error.t] so a file the reader could not find renders
    as a placeholder message instead of failing the whole interface.
    [char_range] underlines those columns of [active_line]. *)
val view
  :  width:int
  -> height:int
  -> file_label:string
  -> source:Loaded.t Or_error.t
  -> active_line:int
  -> callsite_line:int option
  -> char_range:int * int
  -> View.t
