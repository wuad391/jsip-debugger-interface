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
    inline. [Address] points at another tracked structure resolvable through
    the event's registry; [Id] is a registry id boundary. *)
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
  [@@deriving sexp, bin_io, compare, equal]

  (** Source-ish spelling of the value: [42], ["a"], [0x1a0], [#2],
      [[|1.; 2.|]] ... — what the heap pane prints inside a node. *)
  val display : t -> string
end

(** Which catalogued data structure the snapshot walked. Constructors mirror
    the compiler's [Data_structure.t]; new tracked structures show up here as
    new constructors. *)
module Ds_type : sig
  type t =
    | Map
    | Set
    | Queue
  [@@deriving sexp, bin_io, compare, equal]

  (** Lowercase, for header chips: ["map"], ["set"], ["queue"]. *)
  val display : t -> string

  (** The masked field labels of one node's kind, in walk order — the
      compiler's [Data_structure.layout] minus what never reaches the wire
      (the AVL height [h], a queue's [last]). A label absent from the node's
      {!Node.t.block} was a walked child: the k-th absence is
      {!Node.t.children}'s k-th node, which is how a reader recovers which
      slot a child hung off. [block] disambiguates queue roots
      ([length]/[first]) from queue cells (numeric [0] = content,
      [1] = next), and marks a walked boxed value — a map-node child whose
          labels are all numeric positions, e.g. a tuple in a data slot — as
          having no DS slots at all. *)
  val masked_labels : t -> block:(string * Block.t) list -> string list

  (** The labels whose [(Int 0)] means an empty pointer ([Empty]/[Nil])
      rather than the number 0 — what the heap pane draws as [∅]. *)
  val nil_labels : t -> string list

  (** Whether a node is a walked boxed value rather than a DS node — all its
      labels are numeric positions (see {!masked_labels}). *)
  val is_value_block : (string * Block.t) list -> bool
end

(** One heap block of the walked structure: its address, its non-pointer
    fields as [(label, block)] pairs (labels come from the compiler's
    per-type layout, e.g. [l]/[v]/[d]/[r] for stdlib [Map]), and the blocks
    its pointer fields reach. *)
module Node : sig
  type t =
    { virtual_address : Address.t
    ; block : (string * Block.t) list
    ; children : t list
    }
  [@@deriving sexp, bin_io, compare, equal]
end

type t =
  { ds_type : Ds_type.t
  ; root_node : Node.t
  }
[@@deriving sexp, bin_io, compare, equal]

(** A placeholder snapshot (an empty [Map]), for pre-populated defaults like
    {!Call.empty}. *)
val empty : t
