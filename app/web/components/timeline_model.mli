(** The timeline strip's math: the run's per-step allocation density bucketed
    into a fixed number of colorable segments, and the fraction ↔ step
    conversions scrubbing needs. Pure, so the strip's shape is
    expect-testable. *)

open! Core

(** Per-segment density in [0, 1]: the PEAK of the steps each segment covers,
    so a one-step burst stays visible however long the run. At most
    {!max_segments} segments, one per step below that. *)
val segments : density:float array -> float array

val max_segments : int

(** The step a click at [fraction] (0 = left edge, 1 = right) jumps to. *)
val step_of_fraction : total:int -> fraction:float -> int

(** How many segments the position has reached — those draw full strength,
    the future dims. *)
val played : total:int -> step:int -> segments:int -> int

(** The index of the segment the position stands in, which the strip caps in
    the accent color. Not [played - 1]: at step 0 nothing is played yet and
    the cap still belongs at the very start. *)
val cursor : total:int -> step:int -> segments:int -> int
