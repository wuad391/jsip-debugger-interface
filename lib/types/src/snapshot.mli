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
end

(** Which catalogued data structure the snapshot walked. Constructors mirror
    the compiler's [Data_structure.t] in name and ORDER (the C walker stores
    the constructor index verbatim), so new tracked structures are appended.

    The [Core_*] constructors cover [Base] and [Core] both: [Core]'s
    [Map]/[Set]/[Queue] types are aliases of [Base]'s, so the walker sees one
    representation. Unlike their stdlib counterparts, [Base]'s [Map] and
    [Set] wrap their tree in a record ([{comparator; tree; length}] for
    [Map]) whose comparator field holds closures — the compiler leaves it
    unmasked, so it never reaches the wire — and [Base.Queue] is an
    [Option_array]-backed circular buffer, not a linked chain. *)
module Ds_type : sig
  type t =
    | Map (** stdlib [Map]: [Node {l; v; d; r; h}] tree blocks *)
    | Set (** stdlib [Set]: [Node {l; v; r; h}] tree blocks *)
    | Queue
    (** stdlib [Queue]: [{length; first; last}] root, [Cons] cell chain *)
    | Core_map
    (** [Base]/[Core] [Map]: [{comparator; tree; length}] root over
        [Leaf (k, v)] / [Node (l, k, v, r, h)] tree blocks *)
    | Core_set
    (** [Base]/[Core] [Set]: wrapper record over [Leaf v] /
        [Node (l, v, r, h, size)] tree blocks *)
    | Core_queue
    (** [Base]/[Core] [Queue]: [{num_mutations; front; mask; length; elts}]
        root; [elts] is the backing [Option_array] *)
  [@@deriving sexp, bin_io, compare, equal]
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
