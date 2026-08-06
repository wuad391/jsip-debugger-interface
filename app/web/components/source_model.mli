(** The source pane's rows, as data: {!Jsip_parsing.Syntax}-colored lines,
    fold regions, the active line with the event's [char_range] marked — the
    TUI {!Jsip_tui_components.Source_pane}'s reading minus the terminal
    cells, for the web view to render and tests to read. *)

open! Core
open Jsip_types
module Syntax := Jsip_parsing.Syntax

(** A source file with its highlighting and fold regions precomputed — once
    per file at startup. A region is a top-level definition: a column-0 line
    and everything under it until the next one. *)
module Loaded : sig
  type t =
    { file : Source_file.t
    ; spans : (Syntax.Token.t * string) list Array.t
    ; regions : (int * int) list
    (** [(start, stop)] line spans, 1-based inclusive *)
    ; regions_by_start : int Int.Map.t
    }

  val of_source_file : Source_file.t -> t
end

module Row : sig
  type t =
    | Code of
        { number : int
        ; is_active : bool
        ; is_callsite : bool
        ; region_start : bool
        ; folded : bool
        ; spans : (Syntax.Token.t * string * bool) list
        (** [(token, text, in_char_range)]; the range only marks pieces of
            the active line *)
        }
    | Folded_marker of
        { start : int
        ; stop : int
        ; hides_active : bool
        (** the fold is standing in for the active line, so it takes the wash
            in its place *)
        }
  [@@deriving sexp_of]
end

(** Every line of the file in order, folded regions collapsed to their first
    line plus a marker row. [folds] holds region start lines. *)
val rows
  :  Loaded.t
  -> folds:Int.Set.t
  -> active_line:int
  -> callsite_line:int option
  -> char_range:int * int
  -> Row.t list
