(** A pane: a title row over its body, on the panel surface.

    No box around it — {!Layout}'s dividers are the only lines between panes,
    so the screen reads as one surface split up rather than a grid of boxes:

    {v
     CALL STACK                       5 calls · 2 live
     M.add "a" 1 M.empty                   map.ml:7
     ...
    v} *)

open! Core

(** Exactly [width * height] cells: the title row, then [body] in the
    remaining rows, inset one column each side ({!inner_width}) and cropped
    if it overflows. *)
val view
  :  title:string
  -> meta:string
  -> width:int
  -> height:int
  -> Bonsai_term.View.t
  -> Bonsai_term.View.t

(** How many rows the title takes — a body gets [height - header_height]. *)
val header_height : int

(** How wide a body should be built for a pane of [width]: one column of
    padding each side is the pane's. *)
val inner_width : width:int -> int

(** [fit view ~width ~height] pins [view] to exactly that box — cropping
    overflow, padding shortfall with transparent cells. *)
val fit : Bonsai_term.View.t -> width:int -> height:int -> Bonsai_term.View.t

(** A [─] run and a [│] column, for the dividers between panes. *)
val horizontal_rule
  :  width:int
  -> color:Bonsai_term.Attr.Color.t
  -> Bonsai_term.View.t

val vertical_rule
  :  height:int
  -> color:Bonsai_term.Attr.Color.t
  -> Bonsai_term.View.t

(** [glyph] repeated [width] times — for multi-byte box characters where
    [String.make] would count bytes. *)
val repeat : string -> width:int -> string
