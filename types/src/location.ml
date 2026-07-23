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

let file_path t = t.file_path
let line_number t = t.line_number
let char_start t = fst t.char_range
let char_end t = snd t.char_range
let char_range t = t.char_range

let create ~file ~line ~char_range =
  { file_path = file; line_number = line; char_range }
;;
