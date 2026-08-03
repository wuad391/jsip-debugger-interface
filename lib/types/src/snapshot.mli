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
    did not decode. *)
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
    the compiler's [Data_structure.t] — order included, since constructor
    order is the C walker's contract; new tracked structures show up here as
    new constructors. *)
module Ds_type : sig
  type t =
    | Map
    | Set
    | Queue
    | Hashtbl
  [@@deriving sexp, bin_io, compare, equal]

  (** Lowercase, for header chips: ["map"], ["hashtbl"], ... *)
  val display : t -> string

  (** One layer of the structure's internal representation, mirroring the
      compiler's [Data_structure.layout] with the masks spelled as label
      lists. A node's [block] holds every masked field it kept inline; a
      masked label absent from it was a walked child, and the k-th such
      absence is the k-th child — that recovery is per layer:

      - [Fixed]: [interior] fields lead one layer deeper into the structure's
        own skeleton, [payload] fields hold user data (walked payload blocks
        and everything below them are generic value blocks with numeric
        positional labels, no masks).
      - [Array_elements]: a variable-size block whose fields get numeric
        labels and are all interior. *)
  module Layer : sig
    type t =
      | Fixed of
          { labels : string list
          ; interior : string list
          ; payload : string list
          }
      | Array_elements
  end

  (** Root first, in the order interior edges meet them; nonempty, and past
      the end the last layer repeats (a map's l/r spine, a bucket chain's
      next). *)
  val layers : t -> Layer.t list
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
