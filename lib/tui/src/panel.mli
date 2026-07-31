(** The bordered box every pane lives in.

    Draws the mockup's panel chrome in box-drawing characters — the title in
    the top border, a right-aligned meta chip, and the panel background —
    then fits [body] inside:

    {v
    ┌ CALL STACK ──────────────── 2 live ┐
    │ ...body...                         │
    └────────────────────────────────────┘
    v} *)

open! Core
module View := Bonsai_term.View

(** Exactly [width * height] cells. [body] gets the inner
    [(width - 2) * (height - 2)] box, cropped if it overflows. [strong] uses
    the mockup's darker border (the source pane's). *)
val view
  :  ?strong:bool
  -> title:string
  -> meta:string
  -> width:int
  -> height:int
  -> View.t
  -> View.t

(** [fit view ~width ~height] pins [view] to exactly that box — cropping
    overflow, padding shortfall. *)
val fit : View.t -> width:int -> height:int -> View.t

(** How wide a body should be built for a panel of [width]: the borders and
    one column of padding each side are the panel's. *)
val inner_width : width:int -> int

(** [glyph] repeated [width] times — for multi-byte box characters where
    [String.make] would count bytes. *)
val repeat : string -> width:int -> string

(** A [─] run, for separators inside panes. *)
val horizontal_rule : width:int -> color:Bonsai_term.Attr.Color.t -> View.t
