open! Core

module Info : sig
  type t =
    { depth : int (** call nesting depth, rebuilt from the dump's markers *)
    ; id : int (** the tracked structure's stable registry id *)
    ; function_info : Function_info.t
    ; location : Location.t
    ; arguments : Argument.t list
    ; registry : (int * Snapshot.Address.t) list
    (** every tracked-and-alive structure at event time, as
        [(id, current address)] pairs; resolves [Address]/[Id] references
        inside [snapshot] *)
    ; snapshot : Snapshot.t (** the walked shape of the structure *)
    }
  [@@deriving sexp_of]
end

type t =
  { info : Info.t
  ; range : int * int
  (** the 0-based, inclusive span of events (indices into
      {!Call_stack.t.call_order}) during which this call is on the stack:
      from its own event until the last event before one at its depth or
      shallower *)
  }
[@@deriving sexp_of]

(* the empty, default call to be populated later *)
val empty : t

(* create a call given info and the call range *)
val create : info:Info.t -> range:int * int -> t
