(** The heap pane: every live tracked structure, each drawn the way a CS
    diagram draws a tree.

    One section per registry entry — a [#id · kind] header (with the
    structure this step's event walked in the highlight blue) over its tree.
    A structure keeps the shape of its most recent walk and only leaves the
    pane when the registry drops it (the GC collected it) — see
    {!Jsip_replay.Replay.Structure}.

    Each node is a card — its meaning up top ([{"a" ↦ 2}] for a map binding,
    the element for a set, [length n] for a queue root, the content for a
    cell, the joined positions for a walked value block), its full address in
    small type below — outlined in the brand gold, with cards allocated at
    this step in the brighter fresh gold plus a [new] chip. A node's card
    sits centered over its children; siblings share a level, and a labeled
    rail connects parent to children. [∅] marks an interior node's empty
    slot; leaves keep empty slots to themselves. Queue cells' numeric wire
    labels print as [v]/[next].

    {v
    ▸ #1 · map
        ┌────────────────────┐
        │ "a" ↦ 1        new │
        │ 0x763be65ee878     │
        └────────────────────┘
         ┌─────────┴────────┐
         l                  r
         ∅        ┌────────────────────┐
                  │ "b" ↦ 2        new │
                  │ 0x763be65ee8a8     │
                  └────────────────────┘
    v} *)

open! Core
open Jsip_types
open Jsip_replay
module View := Bonsai_term.View

val view
  :  width:int
  -> height:int
  -> structures:Replay.Structure.t list
  -> new_addresses:Snapshot.Address.Set.t
  -> scroll:int
  -> View.t

(** The card a click at pane-body position [(x, y)] lands on, mirroring
    [view]'s layout and scrolling — the app jumps the replay to that node's
    allocation step. *)
val address_at
  :  structures:Replay.Structure.t list
  -> new_addresses:Snapshot.Address.Set.t
  -> scroll:int
  -> height:int
  -> x:int
  -> y:int
  -> Snapshot.Address.t option
