open! Core

module Info = struct
  type t =
    { depth : int
    ; function_info : Function_info.t
    ; location : Location.t
    ; arguments : Argument.t list
    }
end

type t =
  { info : Info.t
  ; range : int * int
  }

let empty =
  { info =
      ({ depth = 0
       ; function_info = Function_info.Unnamed "default"
       ; location =
           Location.create ~file_path:"none" ~line_number:0 ~char_range:(0, 0)
       ; arguments = []
       }
       : Info.t)
  ; range = 0, 0
  }
;;

let create ~info ~range = { info; range }
