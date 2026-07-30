(** Word-wrapping for attributed span lists — what lets long call arguments
    and source lines fold instead of cropping.

    Spans are [(tag, text)] pairs; the tag (an attribute, a token kind) rides
    along unchanged so callers re-attach styling per piece. Widths count
    bytes, which matches the panes' column math for the ASCII source the
    dumps carry. *)

open! Core

(** Break at spaces where possible, hard-split words longer than a whole
    line, and drop the space a break lands on. Every returned line's total
    text length is at most [width] (given [width >= 2]; below that the input
    comes back unwrapped). Concatenating the lines loses only the spaces that
    became breaks. *)
val spans : ('a * string) list -> width:int -> ('a * string) list list
