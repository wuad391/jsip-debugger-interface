open! Core

module Address = struct
  module T = struct
    (* [int64], not [nativeint]: js_of_ocaml gives [nativeint] 32 bits, and a
       real heap address needs ~48, so the web interface would reject every
       dump at its first address. On 64-bit native the two are the same word. *)
    type t = int64 [@@deriving bin_io, compare, equal, hash]

    (* [%string] has no hex conversion, and [Int64.Hex] signs addresses past
       2^63, so this stays [Printf]. One definition, because the sexp and the
       on-screen rendering are required to agree. *)
    let display t = Printf.sprintf "0x%Lx" t

    (* The wire prints addresses as [0x...] atoms; mirror that so our sexps
       round-trip byte-for-byte against the compiler's emitter. *)
    let sexp_of_t t = Sexp.Atom (display t)

    let t_of_sexp sexp =
      match sexp with
      | Sexp.List _ ->
        raise_s
          [%message "Snapshot.Address: expected an atom" (sexp : Sexp.t)]
      | Sexp.Atom s ->
        (try Int64.of_string s with
         | _ ->
           raise_s
             [%message "Snapshot.Address: not an address" (sexp : Sexp.t)])
    ;;
  end

  include T
  include Comparable.Make (T)
end

module Block = struct
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

  let display t =
    match t with
    | Int i -> Int.to_string i
    | Float f -> Float.to_string f
    | String s ->
      (* payload strings are raw; escape so a key holding quotes or
         backslashes comes out unambiguous, matching the compiler-escaped
         source text the stack pane shows *)
      [%string {|"%{String.escaped s}"|}]
    | Int32 i -> [%string "%{i#Int32}l"]
    | Int64 i -> [%string "%{i#Int64}L"]
    | Nativeint i -> [%string "%{i#Nativeint}n"]
    | Float_array floats ->
      let body =
        List.map floats ~f:Float.to_string |> String.concat ~sep:"; "
      in
      [%string "[|%{body}|]"]
    | Address address ->
      (* a block the walker chose not to decode (a closure, an abstract or
         custom block) — angle brackets say "opaque", the address says which
         one *)
      [%string "⟨%{Address.display address}⟩"]
    | Id id -> [%string "#%{id#Int}"]
    (* never printed in practice: a [Child] field is an edge, and the pane
       draws the arrow and the node it reaches instead of a value *)
    | Child -> "→"
  ;;
end

module Ds_type = struct
  (* Constructor order is the compiler's [Data_structure.t] order, which is
     the C walker's contract. One entry per REPRESENTATION, not per module:
     every [Make] instance of a map is the same type, and [Core.Linked_queue]
     is a [Stdlib.Queue.t] and arrives as [Queue].

     [Core_*] is not the stdlib entry under another name. Core reaches its
     containers through Base, and a Base map is a record over a tagged tree
     ([tree], then [Leaf]/[Node]) where the stdlib's is the tree itself; a
     Base hashtable is [{table; length}] over an array of AVL trees where the
     stdlib's buckets are cons chains. They share a name and nothing else, so
     they are separate entries here for the same reason they are separate
     entries in the compiler. *)
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

  (* the header chip. Core entries keep the [core.] qualifier: a Core map
     beside a stdlib one is a thing you want to be able to tell apart. *)
  let display t =
    match t with
    | Map -> "map"
    | Set -> "set"
    | Queue -> "queue"
    | Hashtbl -> "hashtbl"
    | Stack -> "stack"
    | Dynarray -> "dynarray"
    | Core_map -> "core.map"
    | Core_set -> "core.set"
    | Core_hashtbl -> "core.hashtbl"
    | Core_hash_set -> "core.hash_set"
    | Core_queue -> "core.queue"
    | Core_stack -> "core.stack"
    | Core_deque -> "core.deque"
    | Core_fdeque -> "core.fdeque"
    | Core_doubly_linked -> "core.doubly_linked"
    | Core_hash_queue -> "core.hash_queue"
    | User -> "user"
  ;;

  (* The one thing the wire cannot say. Every field arrives under its own
     label, and a field holding a walked block reads [Child] — so nothing
     here needs a layout to name anything. But an EMPTY pointer and the
     integer zero are the same word in memory, and both arrive as [Int 0]:
     [l (Int 0)] on a map node is the empty subtree, [d (Int 0)] on the same
     node is the data zero.

     So these are the labels each structure's own skeleton uses for its
     internal pointers, and only those — enough to draw an empty slot as an
     empty box instead of a bare [0]. Deliberately not exhaustive:

     - numeric labels are left out, because an array slot and a payload
       tuple's field both read ["1"] and only one of them is a pointer;
     - a label the structure also uses for user data is left out (a stdlib
       hashtable's root reaches its buckets through [data], and its entries
       hold their value under the same name — and a bucket array is never
       empty, so nothing is lost).

     Getting this wrong costs a box, never a wrong name: an unlisted empty
     slot reads [l=0]. *)
  let interior_labels t =
    match t with
    | Map | Set -> [ "l"; "r" ]
    | Queue -> [ "first" ]
    | Hashtbl -> [ "next" ]
    | Stack -> [ "c" ]
    | Dynarray -> [ "arr" ]
    | Core_map | Core_set -> [ "tree"; "l"; "r" ]
    | Core_hashtbl | Core_hash_set -> [ "table"; "l"; "r" ]
    | Core_queue | Core_stack -> [ "elts" ]
    | Core_deque -> [ "arr" ]
    | Core_fdeque -> [ "front"; "back" ]
    | Core_doubly_linked -> [ "contents"; "next" ]
    | Core_hash_queue -> [ "queue"; "contents"; "next" ]
    (* a user type has no skeleton of its own: its shape came from the schema
       the instrumentation derived, and every field of it is data *)
    | User -> []
  ;;
end

module Node = struct
  type t =
    { id : int
    ; virtual_address : Address.t
    ; block : (string * Block.t) list
    ; children : t list
    }
  [@@deriving sexp, bin_io, compare, equal]

  (* the wire dumps a re-observed immutable structure as its id, its current
     address, and nothing else — the shape is whatever that id was defined as
     earlier in the dump *)
  let is_revisit_stub t = List.is_empty t.block && List.is_empty t.children

  (* Pre-order over the whole tree. Every caller that wants to index, count
     or collect from a dumped structure goes through this rather than writing
     the recursion again — a new field on [t] then reaches them all. *)
  let rec fold t ~init ~f =
    List.fold t.children ~init:(f init t) ~f:(fun acc child ->
      fold child ~init:acc ~f)
  ;;

  (* Sizing what the wire recorded: each node is a block — one header word
     plus a word per field — and the payloads the wire can size add their own
     blocks: a string is a header plus its padded bytes, a boxed number is a
     custom block, a float array is a header plus a word per element. A block
     whose every field is a float counts flat, the way OCaml lays float
     records out. Undecoded pointers ([Address], closures) count their slot
     alone, and an [Id] is counted where its node was defined — a floor on
     the walked shape, not a census of everything reachable. *)
  let heap_words t =
    fold t ~init:0 ~f:(fun words (node : t) ->
      let flat_floats =
        (not (List.is_empty node.block))
        && List.for_all node.block ~f:(fun ((_ : string), block) ->
          match (block : Block.t) with
          | Float (_ : float) -> true
          | Int _ | String _ | Int32 _ | Int64 _ | Nativeint _
          | Float_array _ | Address _ | Id _ | Child ->
            false)
      in
      let payload (block : Block.t) =
        match block with
        | Float (_ : float) ->
          (match flat_floats with true -> 0 | false -> 2)
        | String s -> 1 + ((String.length s + 8) / 8)
        | Int32 _ | Int64 _ | Nativeint _ -> 3
        | Float_array floats -> 1 + List.length floats
        | Int _ | Address _ | Id _ | Child -> 0
      in
      words
      + 1
      + List.length node.block
      + List.sum (module Int) node.block ~f:(fun ((_ : string), block) ->
        payload block))
  ;;
end

type t =
  { ds_type : Ds_type.t
  ; root_node : Node.t
  }
[@@deriving sexp, bin_io, compare, equal]

let empty =
  { ds_type = Ds_type.Map
  ; root_node =
      { Node.id = 0; virtual_address = 0L; block = []; children = [] }
  }
;;
