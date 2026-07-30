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
    | String s -> [%string {|"%{s}"|}]
    | Int32 i -> [%string "%{i#Int32}l"]
    | Int64 i -> [%string "%{i#Int64}L"]
    | Nativeint i -> [%string "%{i#Nativeint}n"]
    | Float_array floats ->
      let body =
        List.map floats ~f:Float.to_string |> String.concat ~sep:"; "
      in
      [%string "[|%{body}|]"]
    | Address address -> Address.display address
    | Id id -> [%string "#%{id#Int}"]
  ;;
end

module Ds_type = struct
  type t =
    | Map
    | Set
    | Queue
  [@@deriving sexp, bin_io, compare, equal]

  let display t =
    match t with Map -> "map" | Set -> "set" | Queue -> "queue"
  ;;

  (* The masked field labels of one node's kind, in walk order — the
     compiler's [Data_structure.layout] minus the fields that never reach the
     wire (the AVL height [h], a queue's [last]). A label absent from a
     node's [block] was a walked child; the k-th absence is [children]'s k-th
     node. Queue roots ([{length; first}]) and cells (numeric 0 = content, 1
     = next) share a [ds_type]; the root is the node whose block carries
     [length] or [first]. *)
  (* a walked boxed value (a tuple in a map's data slot, say) is a generic
     scanned block, not a DS node: its fields get numeric positional labels
     and none of them are DS slots *)
  let is_value_block block =
    (not (List.is_empty block))
    && List.for_all block ~f:(fun (label, (_ : Block.t)) ->
      String.for_all label ~f:Char.is_digit)
  ;;

  let masked_labels t ~block =
    match t with
    | (Map | Set) when is_value_block block -> []
    | Map -> [ "l"; "v"; "d"; "r" ]
    | Set -> [ "l"; "v"; "r" ]
    | Queue ->
      let is_root =
        List.exists block ~f:(fun (label, (_ : Block.t)) ->
          String.equal label "length" || String.equal label "first")
      in
      (match is_root with
       | true -> [ "length"; "first" ]
       | false -> [ "0"; "1" ])
  ;;

  (* labels whose [(Int 0)] is an empty pointer ([Empty]/[Nil]), not the
     number 0 *)
  let nil_labels t =
    match t with Map | Set -> [ "l"; "r" ] | Queue -> [ "first"; "1" ]
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
