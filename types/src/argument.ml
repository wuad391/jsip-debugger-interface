open! Core

module T = struct
  type t =
  | No_label of {
    expression : Function.Unnamed.t 
  }
  | Labelled of {
    label : string
    expression : Function.Unnamed.t 
  }
  | Optional of {
    label : string
    expression : Function.Unnamed.t 
  }
  [@@deriving sexp, bin_io, , compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)