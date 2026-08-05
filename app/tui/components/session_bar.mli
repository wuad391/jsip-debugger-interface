(** The one-row session bar across the bottom of the screen.

    App chip, then the dump's name and the structure being replayed — where
    the run is in the replay lives in the transport strip up top, not here.

    The right end is the app's cross-pane legend, the one place that says
    what the two ambient color scales mean: [stack █████ compute] (the call
    stack's callee names carry each function's share — of a loaded perf
    profile, or of the trace's own events, and the label says which) and
    [timeline ▀▀▀ ▀▀▀ alloc] (the tick strip's cells brighten with how much
    their steps allocated, within the hue that already means past or future).
    Whole chips only: the pair where they fit, the stack chip alone on a
    narrower screen, nothing on a narrower one still. *)

open! Core
module View := Bonsai_term.View

(** [structure] is the {!Snapshot.Ds_type} chip; [heat] labels the stack chip
    — [`Compute] for a loaded perf profile's sampled shares, [`Calls] for the
    trace-frequency fallback, [None] drops it (the timeline chip stays). *)
val view
  :  width:int
  -> dump_name:string
  -> structure:string
  -> heat:[ `Compute | `Calls ] option
  -> View.t
