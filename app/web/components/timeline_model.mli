(** The timeline strip's math: the run's per-step allocation density
    bucketed into a fixed number of colorable segments, and the fraction ↔
    step conversions scrubbing needs. Pure, so the strip's shape is
    expect-testable. *)

open! Core

(** Per-segment density in [0, 1]: the PEAK of the steps each segment
    covers, so a one-step burst stays visible however long the run. At most
    {!max_segments} segments, one per step below that. *)
val segments : density:float array -> float array

val max_segments : int

(** The step a click at [fraction] (0 = left edge, 1 = right) jumps to. *)
val step_of_fraction : total:int -> fraction:float -> int

(** The playhead's position for a step, in [0, 1]. *)
val fraction_of_step : total:int -> step:int -> float

(** How many segments the playhead has reached — those draw full strength,
    the future dims. *)
val played : total:int -> step:int -> segments:int -> int
