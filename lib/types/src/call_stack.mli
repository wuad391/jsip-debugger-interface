(** The full call history of one dump, in event order.

    Built once from the parsed dump, this is the timeline the debugger
    replays: step [s] of the replay is [call_order.(s)], and the call stack
    shown at that step is every call whose {!Call.t.range} spans [s]. Ranges
    are computed from the dump's depth markers alone — a frame stays live
    until the next event at its own depth or shallower. *)

open! Core

type t =
  { call_order : Call.t Array.t
  ; live : int list Array.t
  (** indices into [call_order] of the frames live at each step, outermost
      first — filled during the same pass that computes the ranges, so
      {!frames_at} costs the depth of the stack rather than a scan of the
      whole dump *)
  }

val create : parsed_info:Call.Info.t Queue.t -> t

(** Number of events in the dump — the replay's step count. *)
val length : t -> int

(** The call that fired at [step]. Raises if [step] is outside
    [0 .. length t - 1]. *)
val call_exn : t -> step:int -> Call.t

(** Every call live at [step] — the stack to display — outermost first.
    [call_exn t ~step] is always the last element. *)
val frames_at : t -> step:int -> Call.t list
