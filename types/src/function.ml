open! Core

module T = struct
  type t =
  | Function_name of 
    function_name : string
  | Unnamed of 
    function_content : string
  [@@deriving sexp, bin_io, , compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)