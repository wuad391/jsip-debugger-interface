open! Core

module Info : sig
  type t =
    { depth : int (** call nesting depth, rebuilt from the dump's markers *)
    ; id : int (** the tracked structure's stable registry id *)
    ; function_info : Function_info.t
    ; location : Location.t
    ; arguments : Argument.t list
    ; registry : Registry_entry.t list
    (** every tracked-and-alive structure at event time; resolves [Id]
        references inside [snapshot] and carries the latest variable name
        each structure was observed under *)
    ; ty : Type_info.t option [@sexp.option]
    (** the walked structure's static type, straight off the wire; [None] on
        dumps from a compiler predating the field *)
    ; binder : Scope.Binder.t option [@sexp.option]
    (** the binding this event's root is known by — [None] when it was
        observed without a name (a nested call, a wildcard pattern) *)
    ; scope : Scope.t option [@sexp.option]
    (** what each tracked name means where this event fired; [None] on dumps
        from a compiler predating the field *)
    ; snapshot : Snapshot.t (** the walked shape of the structure *)
    }
  [@@deriving sexp_of]
end

type t =
  { info : Info.t
  ; range : int * int
  (** the 0-based, inclusive span of this call's subtree (indices into
      {!Call_stack.t.call_order}). The wire writes an event when its call
      completes, so the children come first and the call's own event CLOSES
      the range: [snd range] is the call itself, and everything from
      [fst range] up to it ran inside it. *)
  }
[@@deriving sexp_of]

(* create a call given info and the call range *)
val create : info:Info.t -> range:int * int -> t
