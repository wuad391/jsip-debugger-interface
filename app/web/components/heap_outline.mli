(** The heap pane's OUTLINE: one step's heap read as an indented tree of
    rows, one per binding or element — the TUI heap pane's own reading
    ({!Jsip_tui_components.Heap_pane}), which the web pane offers beside the
    canvas diagram as a second tab. A file-system browser over the heap.

    Where {!Heap_scene} draws every walked block as a box, the outline names
    what a structure HOLDS. Three rules do it, and none of them knows which
    container it is looking at: a block with nothing of its own to print is
    plumbing (a hashtable's bucket array, a wrapper record) and the things it
    points at take its place; an edge labeled with the structure's own
    skeleton ({!Jsip_types.Snapshot.Ds_type.interior_labels}) carries the
    container on, so what it reaches lists as a SIBLING; and so does an edge
    landing on a block shaped exactly like this one — a map node's subtree, a
    cons cell's tail. What is left nests. An AVL tree reads as its bindings,
    a bucket array as its entries, a record's fields still hang under it.

    Everything else is the diagram's reading spelled as text: a structure
    referenced from another is listed inside its referrer, once, keeping its
    own name and its own lit/faded verdict; a block already listed is pointed
    at with [↗] rather than listed twice; a revisit stub the registry could
    not resolve says [null].

    Pure data over one {!Jsip_replay.Replay.Step}, so the view only renders
    rows and the tests read them straight:

    {[
      Heap_outline.rows
        ~structures
        ~nodes
        ~new_addresses
        ~folds
        ~filter:""
        ~sort_by_address:false
        ~accordion:false
      |> List.iter ~f:(fun row -> print_endline (Heap_outline.Row.text row))
    ]}

    Folds are {!Heap_scene.Fold_key}s and name the same box in both views, so
    folding a row here folds its box on the canvas; [filter],
    [sort_by_address] and [accordion] mean exactly what they mean to
    {!Heap_scene.build}.

    The rules restated here are {!Jsip_tui_components.Heap_pane}'s, whose
    comments are their long form. They cannot be shared: that library links
    [bonsai_term], which js_of_ocaml cannot compile — the same reason
    {!Heap_scene} restates the diagram's. *)

open! Core
open Jsip_types
open Jsip_replay

(** The pieces a row's value is made of, so the view decides colors once.
    Text is already flattened to one line; [Arrow], [Nothing] and [Gap] are
    the outline's own punctuation and carry their spacing with them. *)
module Span : sig
  module Kind : sig
    type t =
      | Key (** a binding's key, the ["b"] of ["b" → 2] *)
      | Value
      | Label (** a field label, [length ] or [tl=] *)
      | Arrow
      | Nothing (** the wire had nothing to say — spelled out, not dotted *)
      | Gap (** the two spaces between a record's fields *)
    [@@deriving sexp_of, equal]
  end

  type t = Kind.t * string [@@deriving sexp_of, equal]

  (** The spans as plain text. *)
  val text : t list -> string
end

(** One visible row: what reached it, what it is, what it holds, and where it
    sits. Columns are drawn in field order — [field], [name], [ty], [value],
    [stats] — two spaces apart, with the empty ones dropped; a plain entry
    carries only [value], a structure's own row carries all of them. *)
module Row : sig
  type t =
    { depth : int (** 0 for a structure's own row *)
    ; guide : string
    (** the assembled [├─ ]/[└─ ] run, ancestors' bars and all, so the tree
        is drawn from one string *)
    ; field : string (** the edge label that reached this row, or [""] *)
    ; name : string (** a structure's name; [""] on a plain entry *)
    ; ty : string
    ; value : Span.t list
    ; stats : string
    (** a structure's size — nodes, the memory their blocks pin, and the
        visibility note where it has one *)
    ; address : Snapshot.Address.t option
    (** [None] only on an unresolved [↗]. Clicking a row means this address:
        jump to where it was allocated, and pin it. *)
    ; fold : Heap_scene.Fold_key.t (** the box this row's fold toggles *)
    ; foldable : bool
    (** whether the row draws a glyph: it must have rows to hide, and must
        not share its box with the structure row above it *)
    ; folded : bool
    ; hidden : int (** rows tucked under this one — the [⋯ n] while folded *)
    ; is_new : bool (** allocated at this step *)
    ; is_current : bool (** the structure this step's event walked *)
    ; is_pointer : bool (** an [↗] at a block listed elsewhere *)
    ; faded : bool (** the owning structure's name no longer reaches it *)
    ; matched : bool (** true when no filter is set *)
    }
  [@@deriving sexp_of]

  (** [▾]/[▸] where the row can fold, [None] where it cannot. *)
  val glyph : t -> string option

  (** The whole row as one line, guide and glyph column included — what the
      tests read and what a title attribute says. *)
  val text : t -> string
end

(** One step's outline, top to bottom, with the folded rows' contents left
    out. [filter] dims non-matching rows rather than dropping them, the way
    the canvas dims non-matching boxes: a structure whose header matches
    lights its whole subtree, and any row whose own text matches lights
    itself. [accordion] folds every structure but the one the step walked. *)
val rows
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Heap_scene.Fold_key).t
  -> filter:string
  -> sort_by_address:bool
  -> accordion:bool
  -> Row.t list
