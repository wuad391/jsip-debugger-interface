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
    if it overflows. Every cell is filled, so a panel is opaque — one drawn
    over the others hides them. [bg] is the surface it fills with, and only
    the pop-out has cause to pass anything but the default {!Theme.bg}.

    [body_size], when the caller already knows the body's dimensions, keeps
    the pane from measuring it — measuring forces the body's image against a
    key the paint pass does not use, and on a canvas the size of the heap's
    that rebuild costs more than the whole layout. *)
val view
  :  ?bg:Bonsai_term.Attr.Color.t
  -> ?body_size:int * int
  -> title:string
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
    overflow, padding shortfall with transparent cells. [size] is the view's
    own dimensions if the caller already knows them; given, the view is not
    measured (see {!view}). *)
val fit
  :  ?size:int * int
  -> Bonsai_term.View.t
  -> width:int
  -> height:int
  -> Bonsai_term.View.t

(** Where two dividers meet — pass the box-drawing glyph. *)
val junction : color:Bonsai_term.Attr.Color.t -> string -> Bonsai_term.View.t

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

(** One row of a scrolling pane: [view] pinned to [width] by {!fit}, washed
    with [bg] when the row is selected. Every pane draws its rows through
    this, so a selected line looks the same in all of them. *)
val row
  :  ?bg:Bonsai_term.Attr.Color.t
  -> Bonsai_term.View.t
  -> width:int
  -> Bonsai_term.View.t
