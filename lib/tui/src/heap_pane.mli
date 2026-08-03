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

    Each node is a card — the structure's name (or [#id]) riding the border's
    top left, and its meaning up top ([{"a" → 2}] for a map binding, the
    element for a set, [length n] for a queue root, the content for a cell,
    the joined positions for a walked value block). A card allocated at this
    step carries a green [new] tag riding the border's top right, and a
    folded card says how many nodes it hides below itself.

    Two of the cards are picked out (see {!Selection}): the selected one is
    blue and is the only card that spells out its address — twelve hex digits
    on every card set the whole diagram's width from a string nobody was
    reading — and the one the keyboard is aiming at is orange. Everything
    else outlines in the calmer card blue.

    {v
    ▾ m · map ⟨string ⇒ int⟩
    ▾┌ m ──────────── new ┐
     │ "a" → 1            │
     │ 0x763be65ee878     │  ← selected: blue, and the address shows
     └────────────────────┘
      ┌─────────┴────────┐
      l                  r
      ∅        ▸┌ ───── new ┐
                │ "b" → 2   │  ← orange while the cursor is here
                └───────────┘
                  ⋯ 2 hidden
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

(** The chosen card and the one the keyboard is aiming at, both keyed by
    address — what a click yields and what the canvas keys cards by.

    They coexist: [selected] stays blue while [cursor] moves in orange, so
    you can see where you came from and where [Enter] would take you.
    Committing makes the cursor the selection and clears it.

    Both are geometry: the two picked-out cards are a row taller and several
    columns wider than the rest, being the only ones that spell out an
    address. {!move_cursor} therefore reads the tree rather than the
    drawing, so aiming does not depend on where the last press left the
    picture. *)
module Selection : sig
  type t =
    { selected : Snapshot.Address.t option
    ; cursor : Snapshot.Address.t option
    }
  [@@deriving sexp_of, equal]

  val none : t
end

module Direction : sig
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, equal]
end

val view
  :  width:int
  -> height:int
  -> structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> selection:Selection.t
  -> View.t

(** Where [wasd] lands from wherever the cursor is (or, failing that, the
    selection). The cursor walks the tree rather than the picture: [Up] and
    [Down] climb to a card's parent and descend to its first child, [Left]
    and [Right] run along the layer it sits on — cousins included, since a
    layer is a depth in one tree, not one parent's children. Empty slots and
    [↗ #n] pointers place no card and so are skipped.

    Structures stack down the canvas, so the ends join up: [Up] from a root
    climbs to the tree above it, [Down] from a leaf drops into the one below.

    [None] when nothing lies that way; with no cursor and no selection, the
    first tree's root, so the first keypress always lands somewhere. *)
val move_cursor
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> selection:Selection.t
  -> direction:Direction.t
  -> Snapshot.Address.t option

(** Where a card sits on the canvas — its top row and height, before
    scrolling — so the app can bring the cursor into view. *)
val row_of
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> selection:Selection.t
  -> Snapshot.Address.t
  -> (int * int) option

(** The fold glyph a click at pane-body position [(x, y)] hits, mirroring
    [view]'s layout and scrolling. Checked before {!address_at}: glyph cells
    toggle, the rest of a card jumps. *)
val toggle_at
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> selection:Selection.t
  -> height:int
  -> x:int
  -> y:int
  -> Fold.t option

(** The card a click at pane-body position [(x, y)] lands on, mirroring
    [view]'s layout and scrolling — the app jumps the replay to that node's
    allocation step. *)
val address_at
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> selection:Selection.t
  -> height:int
  -> x:int
  -> y:int
  -> Snapshot.Address.t option
