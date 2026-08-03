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

    Structures lay side by side rather than stacking, up to three to a row,
    wrapping when the next one would not fit and giving a tree wider than the
    pane a row to itself. Two versions of a map beside each other is the
    comparison this pane exists to make.

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

    An edge into a card the canvas already drew is a card too — dashed, and
    named by what that card holds rather than by the wire's node number, so a
    shared subtree reads [↗ "b" → 2] and not [↗ #11].

    Two of the cards are picked out (see {!Selection}): the selected one is
    blue and is the only card that spells out its address — twelve hex digits
    on every card set the whole diagram's width from a string nobody was
    reading — and the one the keyboard is aiming at is orange. Everything
    else outlines in the calmer card blue.

    Because a [↗] pointer and the card it names are one node, picking either
    tints the other's border to match — the border only, so the card you are
    actually on is still the one wearing the wash and the address, and the
    drawing does not shift to make room for a second wide card.

    {v
    ▾ m · map ⟨string ⇒ int⟩            ▾ bigger · map ⟨string ⇒ int⟩
    ▾┌ m ──────────── new ┐             ▾┌ bigger ─────── new ┐
     │ "a" → 1            │              │ "c" → 3            │
     │ 0x763be65ee878     │  ← selected  │ 0x763be65ee9a8     │
     └────────────────────┘              └────────────────────┘
      ┌─────────┴────────┐                 ┌────────┴───────┐
      l                  r                 l                r
    ┌┄┄┄┐       ┌ ───── new ┐         ┌┄ ↗ ┄┄┄┄┄┐         ┌┄┄┄┐
    ┆ ∅ ┆       │ "b" → 2   │  ← its  ┆ "b" → 2 ┆ ← aimed ┆ ∅ ┆
    └┄┄┄┘       └───────────┘  border └┄┄┄┄┄┄┄┄┄┘   at    └┄┄┄┘
                               turns
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

(** Which drawing of a node a position means. A node can be on the canvas
    twice — its own card, and a [↗] pointer at it from a structure that
    shares it — and those are two places even though they are one object. *)
module Site : sig
  type t [@@deriving sexp_of, equal]
end

(** A node, and which drawing of it. Color follows the address, so standing
    on a pointer lights up the card it points at as well; the keyboard
    follows the site, so it stays on the pointer. *)
module Spot : sig
  type t =
    { address : Snapshot.Address.t
    ; site : Site.t
    }
  [@@deriving sexp_of, equal]
end

(** The chosen card and the one the keyboard is aiming at.

    They coexist: [selected] stays blue while [cursor] moves in orange, so
    you can see where you came from and where [Enter] would take you.
    Committing makes the cursor the selection and clears it.

    Both are geometry: the two picked-out cards are a row taller and several
    columns wider than the rest, being the only ones that spell out an
    address. {!move_cursor} therefore reads the tree rather than the drawing,
    so aiming does not depend on where the last press left the picture. *)
module Selection : sig
  type t =
    { selected : Spot.t option
    ; cursor : Spot.t option
    }
  [@@deriving sexp_of, equal]

  val none : t
end

(** A structure's own root card — where the pane starts you off, and what it
    falls back to before anything is chosen. *)
val spot_of_structure : Replay.Structure.t -> Spot.t

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
    layer is a depth in one tree, not one parent's children. Empty slots
    place no card and so are skipped; a [↗] pointer places a card of its own,
    so the cursor can stand on it — lighting up the card it names, without
    leaving the tree you are reading.

    Structures are the outermost layer: from a root [Left]/[Right] step to
    the structure beside it and [Up] to the one before it, and [Down] off a
    leaf falls through to the structure after.

    [None] when nothing lies that way; with no cursor and no selection, the
    first structure's root, so the first keypress always lands somewhere. *)
val move_cursor
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> selection:Selection.t
  -> direction:Direction.t
  -> Spot.t option

(** The fold glyph a click at pane-body position [(x, y)] hits, mirroring
    [view]'s layout and scrolling. Checked before {!spot_at}: glyph cells
    toggle, the rest of a card jumps. *)
val toggle_at
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> selection:Selection.t
  -> width:int
  -> height:int
  -> x:int
  -> y:int
  -> Fold.t option

(** The card a click at pane-body position [(x, y)] lands on, mirroring
    [view]'s layout and scrolling — the app jumps the replay to that node's
    allocation step. Clicking a [↗] pointer lands on the pointer, not on the
    card it names. *)
val spot_at
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> selection:Selection.t
  -> width:int
  -> height:int
  -> x:int
  -> y:int
  -> Spot.t option
