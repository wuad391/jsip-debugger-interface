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
  ;;
end

module Ds_type = struct
  type t =
    | Map
    | Set
    | Queue
    | Hashtbl
  [@@deriving sexp, bin_io, compare, equal]

  let display t =
    match t with
    | Map -> "map"
    | Set -> "set"
    | Queue -> "queue"
    | Hashtbl -> "hashtbl"
  ;;

  (* One layer of a DS's internal representation, mirroring the compiler's
     [Data_structure.layout] with the masks spelled as label lists.
     [interior] fields point one layer deeper into the structure's own
     skeleton; [payload] fields hold user data; anything else was bookkeeping
     and never reached the wire. [Array_elements] is a variable-size block
     (an array) whose every element is interior. *)
  module Layer = struct
    type t =
      | Fixed of
          { labels : string list
          ; interior : string list
          ; payload : string list
          }
      | Array_elements
  end

  (* the layers of one DS, root first, in the order interior edges meet them;
     nonempty, and past the last layer the last repeats (an interior chain —
     a map's l/r spine, a bucket list's next — keeps its own layer forever) *)
  let layers t : Layer.t list =
    match t with
    | Map ->
      [ Fixed
          { labels = [ "l"; "v"; "d"; "r" ]
          ; interior = [ "l"; "r" ]
          ; payload = [ "v"; "d" ]
          }
      ]
    | Set ->
      [ Fixed
          { labels = [ "l"; "v"; "r" ]
          ; interior = [ "l"; "r" ]
          ; payload = [ "v" ]
          }
      ]
    | Queue ->
      [ Fixed
          { labels = [ "length"; "first" ]
          ; interior = [ "first" ]
          ; payload = [ "length" ]
          }
      ; Fixed
          { labels = [ "0"; "1" ]; interior = [ "1" ]; payload = [ "0" ] }
      ]
    | Hashtbl ->
      [ Fixed
          { labels = [ "size"; "data" ]
          ; interior = [ "data" ]
          ; payload = [ "size" ]
          }
      ; Array_elements
      ; Fixed
          { labels = [ "key"; "data"; "next" ]
          ; interior = [ "next" ]
          ; payload = [ "key"; "data" ]
          }
      ]
  ;;
end

module Node = struct
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

let empty =
  { ds_type = Ds_type.Map
  ; root_node = { Node.virtual_address = 0n; block = []; children = [] }
  }
;;
