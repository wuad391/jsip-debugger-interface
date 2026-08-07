(** The live weak registry as a change against the previous event's — what
    the wire carries since the compiler stopped re-stating every entry on
    every event (the full echo was 90% of a real dump's bytes):

    {[
      (upserts ((3 0x7f2ce89e q) (9 0x7f2cf120))) (drops (4 7))
    ]}

    [upserts] holds the entries that are new since the previous event or
    whose address or name changed, in registry (insertion) order, each in
    {!Registry_entry}'s shape; [drops] the ids whose structure the GC
    collected. Folding upserts and drops into the previous event's registry —
    {!Dump_reader} does — reproduces the full registry at every event. Ids
    never recycle, so the order of the two parts is immaterial; the first
    event's delta is the whole registry. *)

open! Core

type t =
  { upserts : Registry_entry.t list
  ; drops : int list
  }
[@@deriving sexp]
