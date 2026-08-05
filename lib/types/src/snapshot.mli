(** A walked snapshot of one tracked data structure at one event.

    Mirrors the compiler side's wire schema ([vreplay/sexp.ml] in the
    jsip_debugger compiler fork) field-for-field, so the derived sexp readers
    here are exact inverses of the compiler's emitters. Every
    [-visual-replay] event carries one of these as its [(snapshot ...)]
    field; {!Dump_wire} unsexps the surrounding event and {!Call.Info}
    carries the result.

    Example wire payload:

    {[
      (ds_type Map)
        (root_node
           ((virtual_address 0x7f08c0e0)
              (block ((l (Int 0)) (v (String a)) (d (Int 1)) (r (Int 0))))
              (children ())))
    ]} *)

open! Core

(** A heap address captured by the runtime walker. Prints as a [0x...] atom
    exactly like the wire; reads back any form [Int64.of_string] accepts.

    [int64] rather than [nativeint] because js_of_ocaml gives [nativeint]
    32 bits and a heap address needs ~48 — the web interface parses dumps
    in the browser. On 64-bit native the representations coincide. *)
module Address : sig
  type t = int64 [@@deriving sexp, bin_io, compare, equal, hash]

  include Comparable.S with type t := t

  (** The wire spelling, [0x%Lx] — what the interface shows next to every
      heap node. *)
  val display : t -> string
end

(** One field of a walked heap block, under the label the walker gave it —
    the wire carries every kept field, so a reader needs no layout of its own
    to name anything.

    [Child] says the field's value is a block of its own: it stands for the
    next node in the owning node's [children], in order. [Id] references the
    node carrying that wire id — a tracked root, a shared block dumped by an
    earlier event, or an earlier node in this same walk (sharing, a cycle).
    [Address] carries only a block the walker did not decode.

    An empty pointer field arrives as [Int 0], indistinguishable from the
    integer zero; see {!Ds_type.interior_labels}. *)
module Block : sig
  type t =
    | Int of int
    | Float of float
    | String of string
    | Int32 of int32
    | Int64 of int64
    | Nativeint of nativeint
    | Float_array of float list
    | Address of Address.t
    | Id of int
    | Child
  [@@deriving sexp, bin_io, compare, equal]

  (** Source-ish spelling of the value: [42], ["a"], [0x1a0], [#2],
      [[|1.; 2.|]] ... — what the heap pane prints inside a node. A [Child]
      is an edge, not a value, and the pane draws it rather than printing it. *)
  val display : t -> string
end

(** Which catalogued data structure the snapshot walked. Constructors mirror
    the compiler's [Data_structure.t] — order included, since constructor
    order is the C walker's contract; new tracked structures show up here as
    new constructors.

    There is one entry per REPRESENTATION, not per module: every [Make]
    instance of a map is the same type, and [Core.Linked_queue] is a
    [Stdlib.Queue.t] and arrives as [Queue].

    [Core_*] is not the stdlib entry under another name. Core reaches its
    containers through Base, and a Base map is a record over a tagged tree
    where the stdlib's map is the tree itself; a Base hashtable is
    [{table; length}] over an array of AVL trees where the stdlib's buckets
    are cons chains. They share a name and nothing else. *)
module Ds_type : sig
  type t =
    | Map
    | Set
    | Queue
    | Hashtbl
    | Stack
    | Dynarray
    | Core_map
    | Core_set
    | Core_hashtbl
    | Core_hash_set
    | Core_queue
    | Core_stack
    | Core_deque
    | Core_fdeque
    | Core_doubly_linked
    | Core_hash_queue
    | User
  [@@deriving sexp, bin_io, compare, equal]

  (** Lowercase, for header chips: ["map"], ["core.hash_queue"], ... — Core
      entries keep the qualifier, because a Core map beside a stdlib one is a
      thing you want to be able to tell apart. *)
  val display : t -> string

  (** The labels this structure's own skeleton uses for its internal pointers
      — the one thing the wire cannot say.

      Every field arrives labeled, and a field holding a walked block reads
      {!Block.Child}, so naming needs no layout. But an empty pointer and the
      integer zero are the same word in memory and both arrive as [Int 0]:
      [l (Int 0)] on a map node is the empty subtree, [d (Int 0)] on the same
      node is the data zero. This is what lets the heap pane draw the first
      as an empty slot and print the second as a value.

      Deliberately not exhaustive — numeric labels are excluded (an array
      slot and a payload tuple's field both read ["1"]), as is any label the
      structure also uses for user data. An unlisted empty slot reads [l=0]:
      the cost of being wrong here is a box, never a wrong name. *)
  val interior_labels : t -> string list
end

(** One heap block of the walked structure: its address, its non-pointer
    fields as [(label, block)] pairs (labels come from the compiler's
    per-type layout, e.g. [l]/[v]/[d]/[r] for stdlib [Map]), and the blocks
    its pointer fields reach. *)
module Node : sig
  type t =
    { id : int
    (** wire id, unique across the dump: the node's definition appears once
        and later occurrences are [Block.Id] references to it *)
    ; virtual_address : Address.t
    ; block : (string * Block.t) list
    ; children : t list
    }
  [@@deriving sexp, bin_io, compare, equal]

  (** Whether this is a re-observed structure's stub — its id, its current
      address, and no content, standing for whatever that id was defined as
      earlier in the dump. *)
  val is_revisit_stub : t -> bool

  (** Pre-order fold over the node and its descendants — [f] sees each node
      once, parents before children. Indexing, counting and collecting all go
      through this, so a new field on [t] reaches every traversal:

      {[
        Snapshot.Node.fold root ~init:0 ~f:(fun n (_ : t) -> n + 1)
      ]} *)
  val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a

  (** The words this node's walked shape occupies on the heap: a header and a
      word per field for every block, plus the out-of-line payloads the wire
      can size (strings, boxed numbers, float arrays; an all-float block
      counts flat, the way OCaml lays float records out). Undecoded pointers
      count their slot alone and a shared [Id] is counted at its definition,
      so this is a floor on the walked shape, not a census of everything
      reachable. The dumps come from 64-bit runs: a word is 8 bytes. *)
  val heap_words : t -> int
end

type t =
  { ds_type : Ds_type.t
  ; root_node : Node.t
  }
[@@deriving sexp, bin_io, compare, equal]

(** A placeholder snapshot (an empty [Map]), for tests and pre-populated
    defaults. *)
val empty : t
