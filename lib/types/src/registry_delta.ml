open! Core

(* mirrors the compiler's [Sexp.sexp_of_registry_delta] field for field, so
   the derived reader is its exact inverse; extras stay rejected like every
   nested wire type *)
type t =
  { upserts : Registry_entry.t list
  ; drops : int list
  }
[@@deriving sexp]
