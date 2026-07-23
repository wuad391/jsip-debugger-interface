open! Core

module T = struct
  type t =
    | No_label of { expression : Function.t }
    | Labelled of
        { label : string
        ; expression : Function.t
        }
    | Optional of
        { label : string
        ; expression : Function.t
        }
  [@@deriving sexp, bin_io]
end

include T
