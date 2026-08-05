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

(** The one shade above {!bg}, for the one thing that sits above the panes
    rather than among them: the heap's diagram pop-out. *)
val raised : Attr.Color.t

(** The call stack's alternating rows — one shade off {!bg}, a band only
    against its neighbor, so a wall of forty calls reads as rows instead of
    as a texture. *)
val stripe_bg : Attr.Color.t

(** The brand gold: the session bar's dot. *)
val accent : Attr.Color.t

(** The "allocated at this step" green — the [new] tag in a card's border. *)
val fresh : Attr.Color.t

(** Selection and position — the bright blue that follows the current step
    and frame across the stack, source, heap, and timeline. *)
val highlight : Attr.Color.t

val highlight_bg : Attr.Color.t
val highlight_deep : Attr.Color.t

(** Where the keyboard is, as opposed to where the selection is: the orange
    that marks the pane [Tab] last focused and, inside it, the row [Enter]
    would commit to. Blue says "chosen", orange says "about to be" — the two
    are on screen together while you aim. *)
val cursor : Attr.Color.t

val cursor_bg : Attr.Color.t
val cursor_deep : Attr.Color.t

(** The muted halves of the two washes above, for the OTHER drawing of the
    row being pointed at — a [↗] pointer and the row it names are one node,
    so both light up. Dimmer than the real thing, so the row you are actually
    on still wins. *)
val cursor_echo : Attr.Color.t

val highlight_echo : Attr.Color.t
val text : Attr.Color.t
val secondary : Attr.Color.t
val muted : Attr.Color.t
val faint : Attr.Color.t
val ghost : Attr.Color.t
val border : Attr.Color.t
val hairline : Attr.Color.t
val tick_past : Attr.Color.t

(** The timeline's activity shading: three stops per state, rising density
    brightening within the state's own hue — past in the position blue's
    register, future in the idle gray warming toward the accent — so density
    reads as intensity while past/current/future keep meaning. *)
val tick_past_ramp : Attr.Color.t array

val tick_future_ramp : Attr.Color.t array

(** The ramp stop for a density in [0, 1]; only genuine bursts (≥ half the
    run's busiest step) reach the bright stop. *)
val tick_density : Attr.Color.t array -> density:float -> Attr.Color.t

val app_purple : Attr.Color.t

(** The heap's two text registers: a printed type in the calm blue (the same
    register as the source pane's identifiers), a walked value in a warm
    orange dim enough that {!cursor}'s bright orange keeps meaning "aimed". *)
val type_name : Attr.Color.t

val value_text : Attr.Color.t

(** A node's box in the diagram pop-out — a calmer blue than {!highlight}. *)
val card_border : Attr.Color.t

(** The rails joining those boxes. Brighter than {!border}: those lines are
    the diagram's pointers, so they should read ahead of the pane chrome
    rather than behind it. *)
val rail : Attr.Color.t

(* syntax colors for the source pane *)
val keyword : Attr.Color.t
val ident : Attr.Color.t
val string_lit : Attr.Color.t
val number : Attr.Color.t

(** The compute-heat ramp the stack pane's heat cells draw from, cold to hot;
    five stops ending in a red deeper than {!cursor}'s orange. *)
val heat_ramp : Attr.Color.t array

(** The ramp stop for a compute share in [0, 1]. Buckets are log-spaced
    (≥20%, ≥8%, ≥3%, ≥1%, below) because shares are heavy-tailed. *)
val heat : share:float -> Attr.Color.t

(** [fg c] and [fg' c] shorthand [Attr.fg c] and [[ Attr.fg c ]]. *)
val fg : Attr.Color.t -> Attr.t

val fg' : Attr.Color.t -> Attr.t list

module For_testing : sig
  (** The role a color plays, by name — ["ghost"], ["card_border"], … The
      picture tests render through [Notty.Cap.dumb], which drops color
      entirely, so anything about fading has to be checked by reading the
      attributes back and naming them. Falls back to the color's sexp for one
      this module never defined. *)
  val color_name : Attr.Color.t -> string
end
