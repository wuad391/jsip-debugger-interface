(** The one-row session bar across the bottom of the screen.

    App chip, then the dump's name and the structure being replayed — where
    the run is in the replay lives in the transport strip up top, not here. *)

open! Core
module View := Bonsai_term.View

(** [structure] is the {!Snapshot.Ds_type} chip; [heat] adds the
    right-aligned legend for the stack pane's colored callee names, labeled
    by what the ramp measures — [`Compute] for a loaded perf profile's
    sampled shares, [`Calls] for the trace-frequency fallback. *)
val view
  :  width:int
  -> dump_name:string
  -> structure:string
  -> heat:[ `Compute | `Calls ] option
  -> View.t
