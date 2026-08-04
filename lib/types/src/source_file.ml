open! Core

type t = { source : string Array.t }

let of_lines lines = { source = Array.of_list lines }
let length t = Array.length t.source

let line t ~number =
  match number >= 1 && number <= length t with
  | true -> Some t.source.(number - 1)
  | false -> None
;;
