open! Core

module T = struct
  type t =
    { file_path : string
    ; line_number : int
    ; char_range : int * int
    }
  [@@deriving sexp, bin_io, compare, equal]
end

include T

let file_path t = t.file_path
let line_number t = t.line_number
let char_range t = t.char_range

let create ~file_path ~line_number ~char_range =
  { file_path; line_number; char_range }
;;

let display t =
  [%string "%{Filename.basename t.file_path}:%{t.line_number#Int}"]
;;
