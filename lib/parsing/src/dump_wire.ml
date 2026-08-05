open! Core
open Jsip_types

type t =
  { id : int
  ; loc : Location.t
  ; fn : Function_info.t
  ; args : Argument.t list
  ; registry : Registry_entry.t list
  ; ty : Type_info.t option [@sexp.option]
  ; binder : Scope.Binder.t option [@sexp.option]
  ; scope : Scope.t option [@sexp.option]
  ; snapshot : Snapshot.t
  }
[@@deriving sexp] [@@sexp.allow_extra_fields]

let of_event_sexp sexp =
  match (sexp : Sexp.t) with
  | List (Atom "event" :: fields) ->
    Or_error.try_with (fun () -> t_of_sexp (Sexp.List fields))
  | _ ->
    Or_error.error_s
      [%message "Dump_wire: expected an (event ...) sexp" (sexp : Sexp.t)]
;;

let of_string line =
  let%bind.Or_error sexp =
    Or_error.try_with (fun () -> Sexp.of_string line)
  in
  of_event_sexp sexp
;;

(* The wire's field names are the compiler's spelling and [Call.Info.t]'s are
   ours; the rename lives here, beside the record it renames, so a new wire
   field is one edit rather than three. [depth] comes from the [{}] markers
   around the line, which are not part of the event. *)
let to_call_info t ~depth : Call.Info.t =
  { depth
  ; id = t.id
  ; function_info = t.fn
  ; location = t.loc
  ; arguments = t.args
  ; registry = t.registry
  ; ty = t.ty
  ; binder = t.binder
  ; scope = t.scope
  ; snapshot = t.snapshot
  }
;;
