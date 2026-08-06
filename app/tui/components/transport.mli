(** The transport strip across the top: tick timeline and the controls.

    A per-step tick bar running the full width of the screen (past dimmed to
    the same hue, current in the highlight blue, future hairline; clickable
    to jump) over the controls: right-aligned chips, each naming its key
    ([◂ back], [step ▸], [[space] play], [. latest], [↑↓ node], [⏎ diagram],
    [h fold], [z accordion], [/ filter], [f flame], [b web], [q quit]) — the
    row is simultaneously the buttons and the key legend, and every chip is
    clickable. The chips that name a mode (play, accordion, diagram, flame,
    web) light up while it is on.

    The middle of the row swaps with focus: the flame drawer rebinds [z] from
    accordion to zoom, [↑↓] aim its bars, and [⏎] jumps rather than popping
    the diagram, so while it holds the keyboard the chips become [↑↓ bar],
    [z zoom], [Z reset]. A legend that names keys which no longer work is
    worse than no legend. *)

open! Core
module View := Bonsai_term.View

(** What the flame drawer is doing, as far as the chip row cares. *)
module Flame_state : sig
  type t =
    | Shut
    | Open (** on screen, but the keyboard is elsewhere *)
    | Focused
    (** holding the keyboard, so [z] zooms rather than toggling accordion *)
  [@@deriving sexp_of, equal]
end

(** What the web twin is doing, as far as the chip row cares: [Live] lights
    the chip, [Failed] grows the label a [✗]. *)
module Web_state : sig
  type t =
    | Idle
    | Live
    | Failed
  [@@deriving sexp_of, equal]
end

module Button : sig
  type t =
    | Back
    | Step
    | Play
    | Latest
    (** re-lands the heap pane on the structure the current step walked *)
    | Node
    (** aims the focused pane's cursor at the next row — a heap outline row,
        or a flame bar while the drawer holds the keyboard *)
    | Diagram
    (** pops the row's structure out as the diagram it physically is, and
        dismisses it again *)
    | Fold
    | Accordion
    | Filter
    | Flame (** [f]: open and shut the flame drawer *)
    | Web
    (** [b]: hand this replay to the web twin — launch it on the same inputs
        and open the browser at it (see {!Jsip_tui.Web_handoff}) *)
    | Zoom (** [z] while the drawer holds the keyboard *)
    | Reset_zoom (** [Z] while the drawer holds the keyboard *)
    | Quit
  [@@deriving sexp_of, equal]
end

(** [Layout.strip_height] rows tall — the bar and the controls, back to back,
    on the pane surface so the strip reads as one piece with them. [step] is
    0-based. [density] is per-step activity in [0, 1] (the app derives it
    from each step's fresh allocations against the run's busiest step); past
    and future cells brighten with the activity they cover, so busy phases
    read straight off the bar. *)
val view
  :  width:int
  -> step:int
  -> total:int
  -> density:float array
  -> playing:bool
  -> accordion:bool
  -> diagram:bool
  -> flame:Flame_state.t
  -> web:Web_state.t
  -> View.t

(** Which step a click at column [x] of the tick row jumps to. *)
val step_at : width:int -> total:int -> x:int -> int option

(** Which chip a click at column [x] of the controls row hits — the same
    layout math [view] draws with. [playing], [flame] and [web] matter: the
    play and web chips' labels and the whole middle of the row (and so every
    extent) change with them. *)
val control_at
  :  width:int
  -> playing:bool
  -> flame:Flame_state.t
  -> web:Web_state.t
  -> x:int
  -> Button.t option
