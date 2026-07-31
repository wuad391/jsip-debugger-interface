open! Core
open Jsip_types

type t =
  { id : int
  ; loc : Location.t
  ; fn : Function_info.t
  ; args : Argument.t list
  ; registry : Registry_entry.t list
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
