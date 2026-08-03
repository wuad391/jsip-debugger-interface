(** The interface's palette — the design mockup's warm-gray scheme as 24-bit
    terminal colors, re-pitched dark: same hues, lightness inverted.

    Every pane draws from here so the whole screen reads as one surface;
    don't inline raw colors in pane modules. Names follow the mockup's roles:
    [accent] is the stepping gold, [fresh] marks newly allocated heap nodes,
    [ghost]/[faint]/[muted]/[secondary] are the text grays from lightest to
    darkest. *)

open! Core
module Attr := Bonsai_term.Attr

(** The screen's one surface — panes, strip and session bar all sit on it, so
    nothing reads as a slab edge. *)
val bg : Attr.Color.t

(** The brand gold: the session bar's dot. *)
val accent : Attr.Color.t

(** The "allocated at this step" green — the [new] tag in a card's border. *)
val fresh : Attr.Color.t

(** Selection and position — the bright blue that follows the current step
    and frame across the stack, source, heap, and timeline. *)
val highlight : Attr.Color.t

val highlight_bg : Attr.Color.t
val highlight_deep : Attr.Color.t

(** The heap cards' outline — a calmer blue than {!highlight}. *)
val card_border : Attr.Color.t

val text : Attr.Color.t
val secondary : Attr.Color.t
val muted : Attr.Color.t
val faint : Attr.Color.t
val ghost : Attr.Color.t
val border : Attr.Color.t
val border_strong : Attr.Color.t
val hairline : Attr.Color.t
val tick_past : Attr.Color.t
val app_purple : Attr.Color.t

(* syntax colors for the source pane *)
val keyword : Attr.Color.t
val ident : Attr.Color.t
val string_lit : Attr.Color.t
val number : Attr.Color.t

(** [fg c] and [fg' c] shorthand [Attr.fg c] and [[ Attr.fg c ]]. *)
val fg : Attr.Color.t -> Attr.t

val fg' : Attr.Color.t -> Attr.t list
