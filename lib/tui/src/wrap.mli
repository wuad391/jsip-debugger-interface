(** Word-wrapping for attributed span lists — what lets long call arguments
    and source lines fold instead of cropping.

    Spans are [(tag, text)] pairs; the tag (an attribute, a token kind) rides
    along unchanged so callers re-attach styling per piece. Widths count
    bytes, which matches the panes' column math for the ASCII source the
    dumps carry. *)

open! Core

(** Break at spaces where possible, hard-split words longer than a whole
    line, and drop the space a break lands on. The first line is at most
    [first_width] (default [width]) and every later line at most [width] —
    panes use a shorter first line to leave room for a right-aligned chip,
    and narrower continuations to indent them. Both must be at least 2 or the
    input comes back unwrapped. Concatenating the lines loses only the spaces
    that became breaks. *)
val spans
  :  ?first_width:int
  -> ('a * string) list
  -> width:int
  -> ('a * string) list list
