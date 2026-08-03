(** The heap pane: every live tracked structure, each drawn the way a CS
    diagram draws a tree.

    A structure keeps the shape of its most recent walk and only leaves the
    pane when the registry drops it (the GC collected it) — see
    {!Jsip_replay.Replay.Structure}. A field holding a reference to another
    live structure — an [Id] into the registry, or an [Address] matching one
    — links that structure's whole tree in as a child at the reference site,
    its root card tagged [#id ·]; only structures nothing references get
    their own [#id · kind] section header (the one this step's event walked
    in the highlight blue). Each structure is drawn once: a second reference,
    or a cycle, stays an inline [#id]. So in [queue_of_maps], once
    [Queue.add m q] runs, the map's tree hangs off the queue cell's [v→]
    edge.

    Any node folds: a [▾]/[▸] glyph sits in the column before each card with
    something below it (and before each section header); clicking the glyph
    folds that card's children away behind a [⋯ n hidden] border tag — the
    card itself stays — while clicking the header's glyph hides the whole
    structure behind the header, which is exactly its name · kind summary. A
    folded subtree keeps claiming the structures it references, so folding a
    cell does not spill its map back out as a section. Fold keys are stable
    across steps ({!Fold.t}: structure id, or structure id + edge path).

    Each node is a card — its meaning up top ([{"a" ↦ 2}] for a map binding,
    the element for a set, [length n] for a queue root, the content for a
    cell, the joined positions for a walked value block), its full address in
    small type below — outlined in blue; a card allocated at this step
    carries a green [new] tag in its border's top right, and the current
    structure's root card is washed in the highlight background. A node's
    card sits centered over its children; siblings share a level, and a
    labeled rail connects parent to children. [∅] marks an interior node's
    empty slot; leaves keep empty slots to themselves. Queue cells' numeric
    wire labels print as [v]/[next].

    {v
    ▸ m · map
        ┌─────────────── new ┐
        │ m · "a" ↦ 1        │
        │ 0x763be65ee878     │
        └────────────────────┘
         ┌─────────┴────────┐
         l                  r
         ∅        ┌─────────────── new ┐
                  │ "b" ↦ 2            │
                  │ 0x763be65ee8a8     │
                  └────────────────────┘
    v} *)

open! Core
open Jsip_types
open Jsip_replay
module View := Bonsai_term.View

(** What one toggle folds: a whole structure behind its header, or one node's
    children behind its card. Node paths are edge positions from the owning
    structure's root, so folds survive stepping. *)
module Fold : sig
  type t =
    | Structure of int
    | Node of int * int list
  [@@deriving sexp_of, compare, equal]

  include Comparator.S with type t := t
end

val view
  :  width:int
  -> height:int
  -> structures:Replay.Structure.t list
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> View.t

(** The fold glyph a click at pane-body position [(x, y)] hits, mirroring
    [view]'s layout and scrolling. Checked before {!address_at}: glyph cells
    toggle, the rest of a card jumps. *)
val toggle_at
  :  structures:Replay.Structure.t list
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> height:int
  -> x:int
  -> y:int
  -> Fold.t option

(** The card a click at pane-body position [(x, y)] lands on, mirroring
    [view]'s layout and scrolling — the app jumps the replay to that node's
    allocation step. *)
val address_at
  :  structures:Replay.Structure.t list
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> height:int
  -> x:int
  -> y:int
  -> Snapshot.Address.t option
