open! Core

module T = struct
  type t =
    | Function_name of string
    | Unnamed of string
  [@@deriving sexp, bin_io, compare, equal, hash]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)
