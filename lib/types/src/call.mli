open! Core

module Info : sig
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

(* the empty, default call to be populated later *)
val empty : t

(* create a call given info and the call range *)
val create : info:Info.t -> range:int * int -> t
