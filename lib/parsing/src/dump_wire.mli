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
        (registry_delta ((upserts ((1 0x7f08c0e0 m))) (drops ())))
        (ty ((printed "int M.t") (params ((key string) (data int)))))
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
  ; registry : Registry_entry.t list option [@sexp.option]
  (** the live weak registry at event time, whole — ids, current addresses,
      and latest observed variable names. Dumps up to compiler PR #23 state
      it on every event; the delta wire that replaced it leaves it [None].
      Exactly one of this and [registry_delta] is present on a well-formed
      event ({!Dump_reader} enforces it; a transitional compiler emitting
      both is read by this field, the two being equal by construction). *)
  ; registry_delta : Registry_delta.t option [@sexp.option]
  (** the same registry as a change against the previous event — upserts and
      drops, {!Jsip_types.Registry_delta} — which is all the wire carries
      since re-stating every entry per event was measured at 90% of a real
      dump's bytes. [None] on dumps predating the delta. *)
  ; ty : Type_info.t option [@sexp.option]
  (** the static type of this event's walked root; [None] on dumps from a
      compiler predating the field *)
  ; binder : Scope.Binder.t option [@sexp.option]
  (** which [let] the walked root is bound by, when it is bound at all — what
      [scope] has to agree with for the structure to still answer to its name *)
  ; scope : Scope.t option [@sexp.option]
  (** what every tracked name means at the point this event fired; [None] on
      dumps from a compiler predating the field, which is not the same as
      "nothing is in scope" and is why this is an option *)
  ; snapshot : Snapshot.t
  }
[@@deriving sexp]

(** [of_string line] parses one already-unframed dump line (no leading depth
    markers) — the [(event ...)] wrapper the compiler emits. Unknown extra
    fields are ignored so a newer compiler doesn't break the reader. *)
val of_string : string -> t Or_error.t

(** The same event under the interface's own field names, at the nesting
    [depth] the line's [{}] markers put it at. [registry] is the event's FULL
    live registry: the caller resolves it from the wire's own [registry]
    field or by folding [registry_delta] into the previous event's —
    {!Dump_reader} owns that fold — so [Call.Info.t] always carries whole
    registries, never deltas. *)
val to_call_info
  :  t
  -> depth:int
  -> registry:Registry_entry.t list
  -> Call.Info.t
