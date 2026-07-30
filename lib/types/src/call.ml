open! Core

module Info = struct
  type t =
    { depth : int
    ; id : int
    ; function_info : Function_info.t
    ; location : Location.t
    ; arguments : Argument.t list
    ; registry : (int * Snapshot.Address.t) list
    ; snapshot : Snapshot.t
    }
  [@@deriving sexp_of]
end

type t =
  { info : Info.t
  ; range : int * int
  }
[@@deriving sexp_of]

let empty =
  { info =
      ({ depth = 0
       ; id = 0
       ; function_info = Function_info.Unnamed "default"
       ; location =
           Location.create ~file_path:"none" ~line_number:0 ~char_range:(0, 0)
       ; arguments = []
       ; registry = []
       ; snapshot = Snapshot.empty
       }
       : Info.t)
  ; range = 0, 0
  }
;;

let create ~info ~range = { info; range }
