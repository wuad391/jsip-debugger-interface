(** The source pane: the selected frame's file with the mockup's cues.

    Line numbers in a gutter, {!Syntax}-colored code, the active line washed
    in the accent background with a [▎] bar, the caller's line marked [▸]
    when it is in the same file, and the event's [char_range] underlined on
    the active line. Long lines wrap onto continuation rows (blank gutter)
    rather than cropping, top-level definitions fold behind their first line
    plus a [⋯ n lines] marker (a fold hiding the active line takes the wash
    in its place), and the pane scrolls to keep the active line centered.
    [folds] holds region start lines for the displayed file. *)

open! Core
open Jsip_types
module Syntax := Jsip_parsing.Syntax
module View := Bonsai_term.View

(** A source file with its highlighting and fold regions precomputed — do
    this once per file at startup, not per frame. A region is a top-level
    definition: a column-0 line and everything under it until the next one,
    foldable when there is anything to hide. *)
module Loaded : sig
  type t =
    { file : Source_file.t
    ; spans : (Syntax.Token.t * string) list Array.t
    ; regions : (int * int) list
    (** [(start, stop)] line spans, 1-based inclusive *)
    ; regions_by_start : int Int.Map.t
    (** [regions] keyed by its first line — the pane asks "does a region
        start here?" once per line of the file, per frame *)
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
  -> folds:Int.Set.t
  -> active_line:int
  -> callsite_line:int option
  -> char_range:int * int
  -> collapsed:bool
       (** collapsed, the pane renders exactly its title row — [▸] and the
           file chip — which is also the click target that reopens it *)
  -> View.t

(** The fold-region start a click at pane-body position [(x, y)] toggles —
    the [▾]/[▸] gutter cell of a region's first line — or [None] anywhere
    else, mirroring [view]'s wrapping, folding, and scrolling. *)
val toggle_at
  :  width:int
  -> height:int
  -> source:Loaded.t Or_error.t
  -> folds:Int.Set.t
  -> active_line:int
  -> callsite_line:int option
  -> char_range:int * int
  -> x:int
  -> y:int
  -> int option
