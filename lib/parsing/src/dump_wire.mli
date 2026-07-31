(** One [-visual-replay] event as it travels on the wire.

    The compiler emits one event per line (framed by {!Dump_reader}'s [{]/[}]
    depth markers, which are not part of the sexp), every field already in
    the sexp shape [[@@deriving sexp]] gives our own types — so this whole
    record has a derived reader and no string parsing happens anywhere:

    {[
      (event (id 1)
        (loc ((file_path t.ml) (line_number 4) (char_range (10 23))))
        (fn (Function_name M.add))
        (args ((No_label (expression (Unnamed "\"a\"")))
               (No_label (expression (Unnamed m)))))
        (registry ((1 0x7f08c0e0 m)))
        (snapshot ((ds_type Map) (root_node ...))))
    ]}

    The renders live in the compiler's [vreplay/sexp.ml]
    ([sexp_of_loc/fn/args]); the snapshot payload types in
    {!Jsip_types.Snapshot}. {!Dump_reader} turns a [t] plus its
    marker-derived depth into a {!Jsip_types.Call.Info.t}. *)

open! Core
open Jsip_types

type t =
  { id : int (** the walked structure's stable registry id *)
  ; loc : Location.t
  ; fn : Function_info.t
  (** [Function_name] for an ident call like [M.add]; [Unnamed] carries the
      printed source of any other callee. Decided at compile time by the
      instrumentation. *)
  ; args : Argument.t list
  (** every argument keeps its label kind — [No_label], or
      [Labelled]/[Optional] carrying the label. Only the [expression] inside
      is always [Function_info.Unnamed]: the compiler prints each argument as
      source text, even a bare identifier, so [Function_name] never appears
      here. An argument the application was abstracted over reads
      ["OMITTED"]. *)
  ; registry : Registry_entry.t list
  (** the live weak registry at event time — ids, current addresses, and
      latest observed variable names *)
  ; snapshot : Snapshot.t
  }
[@@deriving sexp]

(** Reads the [(event ...)] wrapper the compiler emits. Unknown extra fields
    are ignored so a newer compiler doesn't break the reader. *)
val of_event_sexp : Sexp.t -> t Or_error.t

(** [of_string line] parses one already-unframed dump line (no leading depth
    markers). *)
val of_string : string -> t Or_error.t
