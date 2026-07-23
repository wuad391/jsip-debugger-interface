open! Core

module T = struct
  type t =
    { file_path : string
    ; line_number : int
    ; char_range : int * int
    }
  [@@deriving sexp, bin_io]
end

include T
