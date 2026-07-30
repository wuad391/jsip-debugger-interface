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

module Ds_type = struct
  type t =
    | Map
    | Set
    | Queue
  [@@deriving sexp, bin_io, compare, equal]

  let display t =
    match t with Map -> "map" | Set -> "set" | Queue -> "queue"
  ;;

  (* which block labels are pointer slots, in walk order — an absent one was
     a real pointer and shows up in [Node.children] instead *)
  let pointer_labels t =
    match t with Map | Set -> [ "l"; "r" ] | Queue -> []
  ;;
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
