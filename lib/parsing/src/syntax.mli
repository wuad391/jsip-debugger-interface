(** A small OCaml lexer for the source pane's highlighting.

    Token-level only — enough to color a file the way the design mockup does
    (keywords, module names, strings, numbers, comments), not a real parser.
    [(* *)] nesting is threaded across lines by {!file}, so block comments
    highlight correctly wherever they end. The source pane maps each
    {!Token.t} to a {!Theme} color. *)

open! Core
open Jsip_types

module Token : sig
  type t =
    | Keyword
    | Uident (** capitalized word — module or constructor *)
    | String
    | Number
    | Comment
    | Operator
    | Plain
  [@@deriving sexp_of, compare, equal]
end

(** [line ~comment_depth text] splits one line into [(token, text)] spans
    whose concatenation is exactly [text], and returns the comment depth left
    open at the end of the line. *)
val line : comment_depth:int -> string -> (Token.t * string) list * int

(** Highlight a whole file; index [n] holds line [n + 1]'s spans. *)
val file : Source_file.t -> (Token.t * string) list Array.t
