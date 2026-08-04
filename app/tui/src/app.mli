(** The debugger interface itself: state, wiring, and the event loop.

    Owns the replay position, frame selection, play mode, and heap scroll in
    one state machine; every pane draws from that model. The design mockup's
    interactions map onto:

    - [◂ ▸] / [h l] / [n p] — step the replay; [space] plays it at the
      mockup's cadence, stopping on the last step
    - [↑ ↓] — walk the live frames; the source pane follows the selected
      frame and marks its caller's line
    - clicks — stack rows select, footer ticks and heap nodes jump the replay
      (a node jumps to the step that allocated it), the wheel scrolls the
      heap tree
    - [q] / [Ctrl-C] — leave, restoring the terminal *)

open! Core
open Jsip_replay
open Jsip_tui_components
module View := Bonsai_term.View
module Event := Bonsai_term.Event
module Effect := Bonsai_term.Effect
module Dimensions := Bonsai_term.Dimensions

(** The interface as a [Bonsai_term] app, exposed for tests; [run] is the
    entry point. [sources] is keyed by the file paths the dump's locations
    carry, and [replay] must have at least one step. [profile] is the perf
    heat profile the pipeline captured over the unchanged program; when
    given, the stack pane grows a per-call heat cell and the session bar its
    legend. *)
val component
  :  ?profile:Jsip_types.Heat_profile.t
  -> replay:Replay.t
  -> sources:Source_pane.Loaded.t Or_error.t String.Map.t
  -> dump_name:string
  -> exit:(unit -> unit Effect.t)
  -> dimensions:Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t

(** Runs until [q]/[Ctrl-C]. The terminal is restored on exit. *)
val run
  :  ?profile:Jsip_types.Heat_profile.t
  -> dump_name:string
  -> replay:Replay.t
  -> sources:Source_pane.Loaded.t Or_error.t String.Map.t
  -> unit
  -> unit Async.Deferred.Or_error.t
