open! Core

type t =
  { depth : int
  ; function_info : Function_info.t
  ; location : Location.t
  ; arguments : Argument.t list
  ; call_range : int * int
  }

let empty =
  { depth = 0
  ; function_info = Function_info.Unnamed "default"
  ; location =
      Location.create ~file_path:"none" ~line_number:0 ~char_range:(0, 0)
  ; arguments = []
  ; call_range = 0, 0
  }
;;
