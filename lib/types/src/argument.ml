open! Core

module T = struct
  type t =
    | No_label of { expression : Function_info.t }
    | Labelled of
        { label : string
        ; expression : Function_info.t
        }
    | Optional of
        { label : string
        ; expression : Function_info.t
        }
  [@@deriving sexp, bin_io]
end

include T
