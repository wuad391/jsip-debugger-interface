(** The heap pane: every live tracked structure as an indented outline, one
    line per thing it holds.

    A structure keeps the shape of its most recent walk and only leaves the
    pane when the registry drops it (the GC collected it) — see
    {!Jsip_replay.Replay.Structure}. Structures stack, each one a top-level
    row: its name, its type, and what it holds. A field holding a reference
    to another live structure — an [Id] into the registry, or an [Address]
    matching one — nests that structure under the referring row instead of
    giving it a row of its own, so in [queue_of_maps], once [Queue.add m q]
    runs, the map hangs off the queue element it became. Each structure is
    listed once: a second reference, or a cycle, stays a [↗] row.

    {v
    ▾ q   queue      length 2                              0x796745dee5f8
      ├─ ▾ first  m   int M.t     2 bindings
      │    ├─  "k" → 1
      │    └─  "z" → 4
      └─ ▾ next   xs  int list    2 elements     new
           ├─  7
           └─  9
    v}

    The outline is LOGICAL, not physical. A walked map is an AVL tree and a
    walked hashtable is an array of AVL trees, but neither is what a reader
    is looking for, so three rules — stated on {!val:view}'s implementation
    and needing no per-container knowledge beyond
    {!Jsip_types.Snapshot.Ds_type.interior_labels} — turn a heap shape into
    the contents it stands for:

    - a node with nothing of its own to print is plumbing (a bucket array, a
      wrapper record): the things it points at take its place;
    - an edge whose label is the structure's own skeleton carries on THROUGH
      the container, so what it reaches is a sibling rather than a child;
    - so does an edge landing on a node shaped exactly like its source — a
      cons cell's tail, a map node's subtree.

    So a map lists its bindings, a hashtable its entries, and a list its
    elements, all at one depth, while a record's fields still nest under it.
    Empty skeleton slots are not rows: they are most of a tree's nodes and
    none of its content.

    Any row with something under it folds: a [▾]/[▸] glyph sits before it,
    and clicking the glyph tucks its children away behind a [⋯ n] count while
    the row itself stays. Folding a structure's own row collapses the whole
    structure to that one line. A folded subtree keeps claiming the
    structures it references, so folding a row does not spill its map back
    out as a row of its own. Fold keys are stable across steps ({!Fold.t}:
    structure id, or structure id + edge path).

    Two of the rows are picked out (see {!Selection}): the selected one is
    blue and the one the keyboard is aiming at is orange, and those two are
    the only rows that spell out an address — twelve hex digits on every line
    would set the pane's whole width from a string nobody was reading. The
    address rides the right margin, so revealing one moves nothing to its
    left.

    Because a [↗] row and the row it names are one node, picking either tints
    the other's value to match — the value only, so the line you are actually
    on is still the one wearing the wash and the address. *)

open! Core
open Jsip_types
open Jsip_replay
module View := Bonsai_term.View

(** What one toggle folds: a whole structure behind its own row, or one row's
    children behind it. Node paths are edge positions from the owning
    structure's root, so folds survive stepping. *)
module Fold : sig
  type t =
    | Structure of int
    | Node of int * int list
  [@@deriving sexp_of, compare, equal]

  include Comparator.S with type t := t
end

(** Which drawing of a node a position means. A node can be in the outline
    twice — its own row, and a [↗] row pointing at it from a structure that
    shares it — and those are two places even though they are one object. *)
module Site : sig
  type t [@@deriving sexp_of, equal]
end

(** A node, and which drawing of it. Color follows the address, so standing
    on a [↗] lights up the row it names as well; the keyboard follows the
    site, so it stays on the pointer. *)
module Spot : sig
  type t =
    { address : Snapshot.Address.t
    ; site : Site.t
    }
  [@@deriving sexp_of, equal]
end

(** The chosen row and the one the keyboard is aiming at.

    They coexist: [selected] stays blue while [cursor] moves in orange, so
    you can see where you came from and where [Enter] would take you.
    Committing makes the cursor the selection and clears it.

    Neither is geometry: an address is drawn at the right margin, so picking
    a row moves nothing. {!move_cursor} reads the outline rather than the
    drawing besides, so aiming does not depend on where the pane happens to
    be scrolled. *)
module Selection : sig
  type t =
    { selected : Spot.t option
    ; cursor : Spot.t option
    }
  [@@deriving sexp_of, equal]

  val none : t
end

(** A structure's own row — where the pane starts you off, and what it falls
    back to before anything is chosen. *)
val spot_of_structure : Replay.Structure.t -> Spot.t

(** What [h] folds from this spot — what the glyph beside it already says: on
    a structure's row, the whole structure; on a row inside one, that row's
    children (nothing visible happens on a leaf). Node folds keep working in
    accordion mode, where structure folds are the mode's to decide. *)
val fold_of_spot : Spot.t -> Fold.t

module Direction : sig
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, equal]
end

(** Everything folded but the structure the keyboard is in — the fold set
    accordion mode renders with, recomputed from [selection] every time. That
    is what makes walking the registry open each structure on arrival and
    close it behind you. [folds] passes through underneath: node folds inside
    the open structure keep working, structure folds are overridden (the
    others forced shut, the open one's cleared) while the mode is on and come
    back untouched when it goes off. *)
val accordion_folds
  :  structures:Replay.Structure.t list
  -> folds:Set.M(Fold).t
  -> selection:Selection.t
  -> Set.M(Fold).t

(** Whether the [/] filter keeps a structure: a case-insensitive substring of
    everything the structure says about itself — name, kind, type — so what
    you can see is what you can filter by ([/map], [/order], [/#12]). The
    empty filter keeps everything. *)
val matches_filter : Replay.Structure.t -> filter:string -> bool

(** [scroll] is the row the outline starts at and [pan] the manual horizontal
    offset ([\[]/[\]], or the wheel with ctrl or alt held) — for the rare row
    whose value runs past the pane. Both clamp to the outline; [scroll] also
    slides on its own to keep the aimed row in view.

    [note] rides the meta line ahead of the counts — the app's place to say
    an app-level mode is shaping the pane (the live [/] filter, the accordion
    light). [total] is how many structures there were before the filter; the
    live count reads [n of total] when they differ. *)
val view
  :  note:string option
  -> total:int option
  -> width:int
  -> height:int
  -> structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> pan:int
  -> selection:Selection.t
  -> View.t

(** A spot picked at one step, re-pointed at whatever row draws that node at
    this one — committing a [↗] pointer jumps the replay back to where the
    node was allocated, and the structure the pointer lived in need not have
    existed then. [None] when nothing in the outline is that node. *)
val resolve_spot
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> Spot.t
  -> Spot.t option

(** Where the aiming keys land from wherever the cursor is (or, failing that,
    the selection). The cursor walks the outline the way a file tree walks:
    [Up] and [Down] step to the line above and below — across structure
    boundaries and all, so one key runs the whole pane top to bottom — while
    [Left] climbs to the row this one hangs under and [Right] drops into the
    first row under it.

    A folded row has nothing under it to drop into, which is the point: fold
    what you are done with and [Down] steps past it.

    [None] when nothing lies that way; with no cursor and no selection, the
    first row, so the first keypress always lands somewhere. *)
val move_cursor
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> selection:Selection.t
  -> direction:Direction.t
  -> Spot.t option

(** The fold glyph a click at pane-body position [(x, y)] hits, mirroring
    [view]'s layout and scrolling. Checked before {!spot_at}: the glyph cell
    toggles, the rest of the line jumps. *)
val toggle_at
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> pan:int
  -> selection:Selection.t
  -> width:int
  -> height:int
  -> x:int
  -> y:int
  -> Fold.t option

(** The row a click at pane-body position [(x, y)] lands on, mirroring
    [view]'s scrolling — the app jumps the replay to that node's allocation
    step. A row spans the pane, so only [y] decides. Clicking a [↗] lands on
    the pointer, not on the row it names. *)
val spot_at
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold).t
  -> scroll:int
  -> pan:int
  -> selection:Selection.t
  -> width:int
  -> height:int
  -> x:int
  -> y:int
  -> Spot.t option
