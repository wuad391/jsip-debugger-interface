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
    exactly like the wire; reads back any form [Nativeint.of_string] accepts. *)
module Address : sig
  type t = nativeint [@@deriving sexp, bin_io, compare, equal, hash]

  include Comparable.S with type t := t

  (** The wire spelling, [0x%nx] — what the interface shows next to every
      heap node. *)
  val display : t -> string
end

(** One field of a walked heap block: an immediate or opaque value kept
    inline. [Id] references the node carrying that wire id — a tracked root,
    a shared block dumped by an earlier event, or an earlier node in this
    same walk (sharing, a cycle). [Address] carries only a block the walker
    did not decode. [Child] is the walked pointer itself. *)
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
    (** the field the walker descended through; the node it reaches is the
        next of {!Node.children}, so the two line up in field order:

        {[
          (block ((l (Int 0)) (v (String a)) (d (Int 1)) (r Child)))
            (children (<the node r points at>))
        ]} *)
  [@@deriving sexp, bin_io, compare, equal]

  (** Source-ish spelling of the value: [42], ["a"], [0x1a0], [#2],
      [[|1.; 2.|]] ... — what the heap pane prints inside a node. *)
  val display : t -> string
end

(** Which catalogued data structure the snapshot walked. Constructors mirror
    the compiler's [Data_structure.t] — order included, since constructor
    order is the C walker's contract; new tracked structures show up here as
    new constructors. *)
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
    | User (** a value of a type the program itself declared *)
  [@@deriving sexp, bin_io, compare, equal]

  (** For header chips: ["map"], ["hashtbl"], ["Core.Hash_queue"], and
      ["value"] for {!User}, whose static type says more than a name. *)
  val display : t -> string

  (** What one node of the skeleton is made of. The compiler drops
      bookkeeping before it dumps, so every label on the wire is one of:

      - [interior]: a step deeper into the structure's own skeleton (a map's
        [l]/[r], a bucket chain's [next]);
      - [payload]: the user's data ([v]/[d] on a map node).

      [Elements] is a variable-size block — a bucket array, a ring buffer —
      whose numerically labeled slots are all alike. *)
  module Shape : sig
    type t =
      | Fields of
          { interior : string list
          ; payload : string list
          }
      | Elements of { interior : bool }
  end

  (** The shape of the node carrying exactly [labels]. Identifying a node by
      its own field names rather than by walk depth is what separates a Core
      map's [Leaf] from its [Node], and what keeps a subtree readable when it
      is reached through a shared reference instead of from the root.

      Roles read slots, they do not find them: a walked pointer arrives as
      {!Block.Child} and its node as the next child, so [interior] only says
      which [Int 0] is an empty pointer rather than the number zero.

      {[
        Ds_type.shape Map ~labels:[ "l"; "v"; "d"; "r" ]
        = Fields { interior = [ "l"; "r" ]; payload = [ "v"; "d" ] }
      ]} *)
  val shape : t -> labels:string list -> Shape.t
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
end

type t =
  { ds_type : Ds_type.t
  ; root_node : Node.t
  }
[@@deriving sexp, bin_io, compare, equal]

(** A placeholder snapshot (an empty [Map]), for pre-populated defaults like
    {!Call.empty}. *)
val empty : t
