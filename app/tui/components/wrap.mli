(** Word-wrapping for attributed span lists — what lets long call arguments
    and source lines fold instead of cropping.

    Spans are [(tag, text)] pairs; the tag (an attribute, a token kind) rides
    along unchanged so callers re-attach styling per piece. Widths are
    terminal columns as the renderer counts them, not bytes — the panes wrap
    box-drawing guides, arrows and walked strings, where a byte count would
    break a line early. *)

open! Core

(** Break at spaces where possible, hard-split words longer than a whole line
    (on a code-point boundary, never inside a glyph), and drop the space a
    break lands on.

    A word is what lies between two spaces IN THE TEXT, however many spans it
    spans: a label and its value arrive as two of them with nothing between,
    and [a=] must not end a line with its [1] beginning the next. A change of
    color is not a place to break.

    The first line is at most [first_width] (default [width]) and every later
    line at most [width] — panes use a shorter first line to leave room for a
    right-aligned chip, and narrower continuations to indent them. Both must
    be at least 2 or the input comes back unwrapped. Concatenating the lines
    loses only the spaces that became breaks. *)
val spans
  :  ?first_width:int
  -> ('a * string) list
  -> width:int
  -> ('a * string) list list
