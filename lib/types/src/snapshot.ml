open! Core

module Address = struct
  module T = struct
    type t = nativeint [@@deriving bin_io, compare, equal, hash]

    (* The wire prints addresses as [0x...] atoms; mirror that so our sexps
       round-trip byte-for-byte against the compiler's emitter. *)
    let sexp_of_t t = Sexp.Atom (Printf.sprintf "0x%nx" t)

    let t_of_sexp sexp =
      match sexp with
      | Sexp.List _ ->
        raise_s
          [%message "Snapshot.Address: expected an atom" (sexp : Sexp.t)]
      | Sexp.Atom s ->
        (try Nativeint.of_string s with
         | _ ->
           raise_s
             [%message "Snapshot.Address: not an address" (sexp : Sexp.t)])
    ;;
  end

  include T
  include Comparable.Make (T)

  let display t = Printf.sprintf "0x%nx" t
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
    (** a pointer the walker followed: the node it reaches is
        {!Node.children}'s next one, in field order *)
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
    (* a reader turns this into an edge before it ever prints a value; the
       arrow is what is left if one slips through *)
    | Child -> "→"
  ;;
end

module Ds_type = struct
  (* The compiler's [Data_structure.t] catalogue, constructor for constructor
     — the wire spells these names, so a dump naming one we do not have is a
     hard parse failure, not a degraded display. *)
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

  let display t =
    match t with
    | Map -> "map"
    | Set -> "set"
    | Queue -> "queue"
    | Hashtbl -> "hashtbl"
    | Stack -> "stack"
    | Dynarray -> "dynarray"
    | Core_map -> "Core.Map"
    | Core_set -> "Core.Set"
    | Core_hashtbl -> "Core.Hashtbl"
    | Core_hash_set -> "Core.Hash_set"
    | Core_queue -> "Core.Queue"
    | Core_stack -> "Core.Stack"
    | Core_deque -> "Core.Deque"
    | Core_fdeque -> "Core.Fdeque"
    | Core_doubly_linked -> "Core.Doubly_linked"
    | Core_hash_queue -> "Core.Hash_queue"
    (* a value of the program's own type: the static type on the event says
       far more than a catalogue name would *)
    | User -> "value"
  ;;

  (* What one node of a structure's skeleton is made of. The compiler drops
     bookkeeping before it dumps, so every label that reaches the wire is
     either [interior] — a step deeper into the skeleton — or [payload], the
     user's own data. [Elements] is a variable-size block (a bucket array, a
     ring buffer) whose slots are all alike and numerically labeled. *)
  module Shape = struct
    type t =
      | Fields of
          { interior : string list
          ; payload : string list
          }
      | Elements of { interior : bool }
  end

  (* A node is identified by its own labels rather than by how deep the walk
     is: since the compiler names every kept field, that is enough, and it
     tells apart shapes a depth count cannot — a Core map's two-field [Leaf]
     from its four-field [Node], a hashtable's root from a bucket cell — and
     stays right when a structure is entered through a shared subtree instead
     of from its root.

     Roles are needed only to *read* a slot, never to find one: a walked
     pointer is spelled {!Block.Child}, so [interior] says which [Int 0] is
     the empty pointer rather than the number zero, and [payload] says what a
     card summarizes. *)
  let shape t ~labels : Shape.t =
    let has label = List.mem labels label ~equal:String.equal in
    let is_numeric =
      (not (List.is_empty labels))
      && List.for_all labels ~f:(String.for_all ~f:Char.is_digit)
    in
    (* a cons cell — [hd :: tl] as the walker labels it, position 1 being the
       tail one cell deeper *)
    let cell = Shape.Fields { interior = [ "1" ]; payload = [ "0" ] } in
    match t with
    | Map -> Fields { interior = [ "l"; "r" ]; payload = [ "v"; "d" ] }
    | Set -> Fields { interior = [ "l"; "r" ]; payload = [ "v" ] }
    | Queue when is_numeric -> cell
    | Queue -> Fields { interior = [ "first" ]; payload = [ "length" ] }
    | Stack when is_numeric -> cell
    | Stack -> Fields { interior = [ "c" ]; payload = [ "len" ] }
    | Hashtbl when is_numeric -> Elements { interior = true }
    (* [data] is the bucket array on the root and user data in a cell; only
       the cell carries a [key] *)
    | Hashtbl when has "key" ->
      Fields { interior = [ "next" ]; payload = [ "key"; "data" ] }
    | Hashtbl -> Fields { interior = [ "data" ]; payload = [ "size" ] }
    | Dynarray when is_numeric -> Elements { interior = false }
    | Dynarray -> Fields { interior = [ "arr" ]; payload = [ "length" ] }
    | Core_map ->
      Fields
        { interior = [ "tree"; "l"; "r" ]; payload = [ "length"; "v"; "d" ] }
    | Core_set ->
      Fields { interior = [ "tree"; "l"; "r" ]; payload = [ "length"; "v" ] }
    | (Core_hashtbl | Core_hash_set) when is_numeric ->
      Elements { interior = true }
    | Core_hashtbl | Core_hash_set ->
      Fields
        { interior = [ "table"; "l"; "r" ]
        ; payload = [ "length"; "k"; "v" ]
        }
    | (Core_queue | Core_stack) when is_numeric ->
      Elements { interior = false }
    | Core_queue | Core_stack ->
      Fields { interior = [ "elts" ]; payload = [ "length" ] }
    | Core_deque when is_numeric -> Elements { interior = false }
    | Core_deque -> Fields { interior = [ "arr" ]; payload = [ "length" ] }
    | Core_fdeque when is_numeric -> cell
    | Core_fdeque ->
      Fields { interior = [ "front"; "back" ]; payload = [ "length" ] }
    (* the ref cell, then each element; the [Some] the ref may hold is a
       one-field block whose slot leads to the element *)
    | Core_doubly_linked when is_numeric ->
      Fields { interior = [ "0" ]; payload = [] }
    | Core_doubly_linked ->
      Fields { interior = [ "contents"; "next" ]; payload = [ "v" ] }
    (* the same chain, but an element's [v] steps down to the key/value pair
       that holds the data, so it is interior here *)
    | Core_hash_queue when is_numeric ->
      Fields { interior = [ "0" ]; payload = [] }
    | Core_hash_queue ->
      Fields
        { interior = [ "queue"; "contents"; "v"; "next" ]
        ; payload = [ "key"; "data" ]
        }
    (* a list of the user's own values is the one shape worth knowing: it
       makes an empty tail read as the end of the list, not as zero *)
    | User when has "hd" && has "tl" ->
      Fields { interior = [ "tl" ]; payload = [ "hd" ] }
    (* everything else declared by the program: no skeleton to speak of, so
       every field is data *)
    | User -> Fields { interior = []; payload = labels }
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
end

type t =
  { ds_type : Ds_type.t
  ; root_node : Node.t
  }
[@@deriving sexp, bin_io, compare, equal]

let empty =
  { ds_type = Ds_type.Map
  ; root_node =
      { Node.id = 0; virtual_address = 0n; block = []; children = [] }
  }
;;
