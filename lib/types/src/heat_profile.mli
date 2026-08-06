(** The per-function compute profile the pipeline's perf stage writes
    ([heat.sexp]), and the lookup that joins it onto the replay's calls.

    The visual-debugger shell samples the *unchanged* program — natively
    compiled and looped in-process under [perf] — and aggregates the samples
    per function. This module mirrors that writer's sexp field-for-field (the
    derived [t_of_sexp] is its exact inverse, like {!Dump_wire} is for the
    compiler's dump), e.g.

    {v
    ((version 1)
     (root_module Greet)
     (entries
      (((module_path (Greet)) (kind (Named shout)) (samples 8841))
       ((module_path (Greet)) (kind (Named generate_string)) (samples 3819)))))
    v}

    {!share} answers "what fraction of the sampled OCaml compute ran in the
    function this call names" — the number the TUI turns into a heat color.
    [None] means no profile entry matched; render neutral ("no data"), which
    is different from a cold (near-zero) share.

    Loaded from disk by [Jsip_parsing.Heat_reader]. *)

open! Core

module Kind : sig
  type t =
    | Named of string (** an ordinary [let]-bound function *)
    | Anonymous of
        { file_path : string
        ; line_number : int
        ; char_range : int * int
        } (** a lambda, named by the compiler after its definition site *)
  [@@deriving sexp, equal]
end

module Entry : sig
  type t =
    { module_path : string list
    (** e.g. [["Greet"]] or [["Stdlib"; "Map"]] *)
    ; kind : Kind.t
    ; samples : int
    }
  [@@deriving sexp]
end

type t =
  { version : int
  ; root_module : string
  (** the profiled program's own module; breaks ties when an unqualified
      trace name could be the user's function or a stdlib one *)
  ; entries : Entry.t list
  }
[@@deriving sexp]

(** the {!share} denominator: every sample attributed to OCaml code *)
val total_samples : t -> int

(** Fraction of {!total_samples} spent in the function [function_info] names,
    matching by name (and by [location] — the call site — for [Unnamed]
    callees, against anonymous entries' definition sites). Trace qualifiers
    restrict when they resolve ([M.add] vs [Stdlib.Map]'s [add]) and are
    ignored when they are unresolvable aliases; when several entries survive
    (flambda2 can specialize one source function into several symbols) their
    samples sum. *)
val share
  :  t
  -> function_info:Function_info.t
  -> location:Location.t
  -> float option

(** Fraction of {!total_samples} spent anywhere in the source file the call
    came from — its module's named entries plus lambdas defined in it.

    The coarse reading, for when {!share} declines. Instrumented events fire
    on bindings inside functions, so a callee often arrives as an expression
    ({!Function_info.Unnamed}) located mid-function: no name to match and no
    lambda defined there, and a whole replay can come back neutral even with
    a good profile loaded. The file is still known, and "this call is in the
    module the run spends its time in" is worth saying. Per-file, so every
    call in one file shares a value — use it as a fallback behind {!share},
    never instead of it. [None] when the profile sampled nothing there. *)
val file_share : t -> location:Location.t -> float option
