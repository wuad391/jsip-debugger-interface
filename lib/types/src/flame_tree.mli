(** The dump's calls folded into a flame graph: one node per distinct call
    PATH, widths measured in trace events, costed by the perf profile.

    The tree and the widths come from the REPLAY TRACE — the events in
    {!Call_stack.t.call_order} — not from perf. A bar's width is how many
    calls ran inside it: call VOLUME, not time. {!Heat_profile} is joined on
    afterwards, per function, to say what that volume COST.

    Identical paths pool. Three dynamic calls to [add] under [main] are one
    node with [calls = 3]; that pooling is what makes a flame graph readable,
    and what makes "called four hundred times" visible at a glance.

    {v
    the dump (children first)     the merged tree
      b    depth 2                main  inclusive 5  calls 1
      c    depth 2                ├─ c  inclusive 3  calls 2
      d    depth 3                │  └─ d  inclusive 1  calls 1
      c    depth 2                └─ b  inclusive 1  calls 1
      main depth 1
    v}

    Widths nest exactly. Every trace event is one call, and a call's children
    tile its range with no gaps, so

    {v
      node.inclusive = node.calls + sum of children's inclusive
    v}

    which makes [calls] double as the node's SELF width: the sliver of the
    bar that no child covers.

    The bottleneck numbers live per FUNCTION, in {!Metrics}, not per node —
    {!Heat_profile.share} is a whole-program figure, so every occurrence of a
    function carries the same share. {!Metrics.relative_cost} divides that
    share by the function's call count across the WHOLE trace, and that is
    the number separating the two kinds of bottleneck: a wide bar with a low
    relative cost is a chatty function, fix its caller; a narrow bar with a
    high one is a slow function, fix the function.

    {[
      let tree = Flame_tree.create ~calls ~profile in
      let lit = Flame_tree.path tree ~frames:step.frames in
      (* the merged bars under the replay cursor, outermost first *)
    ]}

    Three things this join is NOT, worth repeating wherever the numbers are
    shown. The trace and the profile come from two different executions of
    the program, so a run whose work depends on timing or input will not line
    up. perf samples an inlined, flambda2-specialized binary while the trace
    names source functions, so a callee that was inlined into its caller has
    its samples charged to the caller. And an unmatched function is [None] —
    "no data", which is not the same as cold. *)

open! Core

(** What makes two calls the same function, for merging and for the perf
    lookup.

    A named function pools across its call sites: {!Heat_profile.share}
    ignores the site for a name, so splitting [List.map] into one node per
    site would spread one number over several bars. A lambda does not pool
    across sites: the profile identifies an anonymous function BY its
    definition site, so two lambdas on two lines are two functions here even
    when their source text matches. *)
module Key : sig
  type t =
    | Named of string (** what {!Function_info.Function_name} carried *)
    | Lambda of
        { text : string (** the source text of an {!Function_info.Unnamed} *)
        ; site : Location.t
        (** where it was written, which perf matches on *)
        }
  [@@deriving sexp_of]

  include Comparable.S_plain with type t := t

  val of_call : Call.t -> t

  (** What a bar is labeled — the same string {!Function_info.display} gives
      for any call that merged into it. *)
  val display : t -> string
end

(** The perf join, one entry per function the trace called.

    Every field but {!calls} is [None] when the run had no profile at all, or
    when the profile matched no entry to this function. *)
module Metrics : sig
  type t =
    { calls : int
    (** how many times the WHOLE trace called this function, summed over
        every node it appears in — including the deeper frames of a
        recursion. This is {!cost_per_call}'s denominator, and it must be
        this total rather than one node's count, because {!share} is itself a
        whole-program number. *)
    ; share : float option
    (** {!Heat_profile.share}: this function's fraction of all the compute
        perf sampled *)
    ; cost_per_call : float option
    (** [share /. calls] — what one invocation costs, still as a fraction of
        the whole run, so it is a tiny number. Exact; for reading, use
        {!relative_cost}. *)
    ; relative_cost : float option
    (** {!cost_per_call} over the average call's, so the scale is
        dimensionless and centered: [1.] is an ordinary call, [20.] a call
        costing twenty ordinary ones, [0.05] one so cheap it can only matter
        in bulk. The average is the matched functions' total share over their
        total calls. *)
    }
  [@@deriving sexp_of]
end

(** One bar. *)
module Node : sig
  type t =
    { key : Key.t
    ; inclusive : int
    (** trace events in this node's subtree, itself included — the bar's
        width, measured in calls made *)
    ; calls : int
    (** dynamic calls merged into this bar; also its self width, since
        [inclusive] less the children's is exactly this *)
    ; first_step : int
    (** the earliest of those calls' own event indices: a stable
        representative, and the step to jump the replay to *)
    ; children : t list
    (** in name order, ties by {!Key.compare}. A flame graph's x-axis is
        alphabetical — not chronological, and not by size — so that the same
        program always draws the same picture and two captures can be read
        side by side. A total order, so a rendering of this tree is an expect
        test. *)
    }
  [@@deriving sexp_of]
end

type t =
  { roots : Node.t list
  (** the calls no other call contains, merged like any other siblings; a
      dump with one top-level call has one root *)
  ; total_events : int
  (** the dump's event count — the roots' widths sum to exactly this *)
  ; functions : Metrics.t Key.Map.t
  }
[@@deriving sexp_of]

(** Fold a dump's calls into the merged tree and join the profile onto it.
    [calls] is {!Call_stack.t.call_order}. Total: an empty dump gives an
    empty forest and an empty table. Linear in the event count, bar the
    per-level sibling sort. *)
val create : calls:Call.t array -> profile:Heat_profile.t option -> t

(** This bar's function's numbers. Total: a node built from this tree is
    always in {!t.functions}, and one from another tree — a caller error —
    gets its own [calls] and no perf numbers rather than an exception. *)
val metrics : t -> Node.t -> Metrics.t

(** {!Metrics.share} split across this function's occurrences, as
    [share *. node.calls /. metrics.calls] — what to color ONE bar by when a
    function appears in several places. An estimate, and a load-bearing one:
    it assumes every call of a function costs the same, which is exactly what
    a recursive or input-sized function does not do. Color with this; rank
    with {!Metrics.relative_cost}. *)
val prorated_share : t -> Node.t -> float option

(** The merged bars the replay's live stack runs through, outermost first —
    the path a renderer lights up. [frames] is {!Call_stack.frames_at},
    equivalently [Replay.Step.frames]. One node per frame for frames from
    this dump; a prefix otherwise. Costs the stack's depth times the
    branching at each level, so it is cheap enough to call on every render. *)
val path : t -> frames:Call.t list -> Node.t list

(** The bar at [path], outermost key first — what a zoomed panel keeps in its
    model and re-resolves each render. [None] for a path naming no bar, and
    for the empty path, which means "not zoomed". *)
val find : t -> path:Key.t list -> Node.t option

(** Every function the profile matched, dearest per call first, ties by
    {!Key.compare} — the "what to fix" list. *)
val by_cost_per_call : t -> (Key.t * Metrics.t) list
