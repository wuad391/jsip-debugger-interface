(** The heap canvas's scene: every live tracked structure as the tree of
    boxes it physically is, ready to lay out and draw.

    This is the TUI diagram pop-out's reading of the wire
    ({!Jsip_tui_components.Heap_pane.Diagram}), promoted to the everyday
    view: the web heap pane draws all structures at once and makes them
    legible by ZOOMING (semantic detail tiers in {!Heap_layout}) where the
    TUI made them legible by flattening into an outline. One box per walked
    block, a dotted [∅] box per empty skeleton slot, a dashed [↗] box where a
    node is already drawn elsewhere (sharing, cycles), and a structure
    referenced from another drawn inline under its referrer with its own name
    and its own lit/faded verdict — each structure appears exactly once.

    Pure data: building is deterministic over one step's
    {!Jsip_replay.Replay.Step}, so it is expect-testable and the canvas
    widget only ever draws it.

    {[
      let roots, stats =
        Heap_scene.build
          ~structures
          ~nodes
          ~new_addresses
          ~folds
          ~filter:""
          ~sort_by_address:false
          ~accordion:false
      in
      List.map roots ~f:(fun root -> root.header, root.count)
    ]} *)

open! Core
open Jsip_types
open Jsip_replay

(** What one fold toggle names: a node by its structure id and edge-index
    path from that structure's root. Paths address the structure's shape, not
    a step's addresses, so folds survive stepping — the TUI's
    {!Jsip_tui_components.Heap_pane.Fold} contract. An inlined referenced
    structure keys folds on its own id, so they follow it whichever structure
    it is drawn inside. *)
module Fold_key : sig
  type t =
    { structure_id : int
    ; path : int list
    }
  [@@deriving sexp_of, compare, equal]

  include Comparator.S with type t := t

  (** The structure's own box — folding it collapses the whole structure. *)
  val root : int -> t
end

module Kind : sig
  type t =
    | Block (** a walked heap block *)
    | Nil (** an empty interior slot, the [∅] box *)
    | Shared of int
    (** a node already drawn: the dashed [↗] pointer box, carrying the wire
        id it points at *)
  [@@deriving sexp_of, equal]
end

(** One content row of a box, in colorable pieces. *)
module Line : sig
  module Part : sig
    type t =
      | Key of string (** a binding's key, e.g. ["b"] of ["b" → 2] *)
      | Value of string
      | Label of string (** a field label, [length ] or [tl=] *)
      | Arrow (** the [→] of a binding *)
      | Nothing (** the wire had nothing to say — spelled out *)
    [@@deriving sexp_of, equal]
  end

  type t = Part.t list [@@deriving sexp_of, equal]

  (** The row as plain text — labels, tooltip lines, searches. *)
  val text : t -> string
end

module Node : sig
  type t =
    { key : Fold_key.t
    ; kind : Kind.t
    ; address : Snapshot.Address.t option
    (** [None] on a [Nil] box, and on a [↗] whose target is unresolved *)
    ; label : string (** detail tier 1's one line — [name ·] first summary *)
    ; lines : Line.t list (** tier 2's field rows *)
    ; raw : (string * string) list
    (** tier 3's machine rows: address, header word, first fields' words *)
    ; words : int (** this block alone: header + one word per field *)
    ; name_tag : string option
    (** the structure's name, on the box that is one's root *)
    ; is_new : bool (** allocated at this step — pulsed and tagged *)
    ; faded : bool (** the owning structure's name no longer reaches it *)
    ; matched : bool (** true when no filter is set *)
    ; folded : bool
    ; hidden_count : int (** direct children a fold is hiding *)
    ; children : (string * t) list (** edge label, child — field order *)
    }
  [@@deriving sexp_of]

  (** Pre-order over the node and its descendants. *)
  val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a
end

(** One top-level structure: its header line and its tree. *)
module Root : sig
  type t =
    { structure_id : int
    ; header : string (** [name · kind type], plus the visibility note *)
    ; note : string option (** [shadowed] / [out of scope], for the header *)
    ; count : int (** block boxes drawn under this root *)
    ; words : int (** {!Snapshot.Node.heap_words} of the walked shape *)
    ; faded : bool
    ; matched : bool (** some box under this root matches the filter *)
    ; is_current : bool (** the structure this step's event walked *)
    ; node : Node.t
    }
  [@@deriving sexp_of]
end

module Stats : sig
  type t =
    { structures : int
    ; nodes : int (** block boxes on the canvas *)
    ; new_nodes : int
    ; hits : int (** matched boxes; 0 while no filter is set *)
    }
  [@@deriving sexp_of, equal]
end

(** The [/] filter's match, structure-level: a case-insensitive substring of
    everything the header says — name, kind, type, visibility note — so
    [/map], [/order], [/#12] and [/shadowed] all find what the outline's
    filter found. *)
val matches_filter : Replay.Structure.t -> filter:string -> bool

(** The [o] ordering: ascending addresses, so memory locality reads as
    adjacency. Registry (creation) order is the default. *)
val by_address : Replay.Structure.t list -> Replay.Structure.t list

(** The scene for one step. [filter] dims non-matching boxes rather than
    removing them (the canvas keeps its shape under a live filter; the
    design's reading) — {!Root.matched}/{!Node.matched} carry the verdicts
    and {!Stats.hits} the count. [accordion] folds every structure to its
    root box except the one the step walked. [folds] are the user's manual
    toggles, applied after the walk so what nests where never depends on
    them. *)
val build
  :  structures:Replay.Structure.t list
  -> nodes:Replay.Nodes.t
  -> new_addresses:Snapshot.Address.Set.t
  -> folds:Set.M(Fold_key).t
  -> filter:string
  -> sort_by_address:bool
  -> accordion:bool
  -> Root.t list * Stats.t
