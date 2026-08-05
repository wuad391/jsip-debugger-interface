(** The flame drawer: the whole run's call tree as nested bars, with the
    replay's live stack lit.

    It sits under the heap, in the right-hand column, and [f] opens and shuts
    it. Shut, it keeps its title row — enough to say it is there and to be
    clicked open — and gives every other row back to the heap. Open, it is a
    pane like any other: [Tab] reaches it, [wasd] aims inside it, [Enter]
    commits.

    One row per call depth, the roots along the BOTTOM and their callees
    stacked above them — the flames rise, which is the shape Brendan Gregg's
    flame graphs have and the reason they are called that. Boxes are filled,
    the label riding on the fill, and siblings run left to right in NAME
    order rather than by size: that is what makes the x-axis alphabetical, so
    the same program always draws the same picture and two captures of it can
    be read side by side.

    A box's width is its subtree's share of the trace's events — CALL VOLUME,
    not time — and its color is that function's share of the perf profile's
    sampled compute. Gregg colors boxes at random, from a warm palette,
    purely to separate neighbours; this spends the channel on the profile
    instead, and does it in blue ({!Theme.flame_ramp}), so the two channels
    are different measurements on purpose: a wide pale box is a chatty
    function, a narrow bright one is a slow function, and telling those apart
    is what the drawer is for ({!Jsip_types.Flame_tree.Metrics.relative_cost}
    puts a number on it). With no [-perf-file] there is no compute to show,
    so the color falls back to call volume and the meta line says
    [color = calls] instead of [color = compute] — the same disclosure the
    session bar's ramp legend makes.

    Children tile their parent exactly — {!Jsip_types.Flame_tree}'s invariant
    is [inclusive = calls + sum of children's inclusive] — so every column of
    a box is accounted for on the row above it, and the gap at that row's
    right end is the parent's own [calls]: the work that ran in the function
    itself rather than in anything it called. That gap is a flame graph's
    exposed top edge, which is exactly where self time lives, so it is drawn
    as nothing at all rather than as a box of its own. A leaf simply ends.
    Children too narrow to draw pool into one [+N] marker instead of
    vanishing — on a view whose job is finding where the work went, dropping
    work silently is the one unaffordable bug.

    {v
     ▾ FLAME     1284 events · width = calls · color = compute
                ▏Map.add    Map.b⋯
      ▏Book.add          Map.find    +6
      ▏run_trades                  setup
     ▏main
    v}

    (The color is the drawing: each label there sits on a filled box running
    the width its subtree earned, and the gaps are exposed self time.)

    The path the replay is standing in right now — the live call stack, its
    outermost frame on the bottom row — carries a [▏] inside each of its
    boxes, and its deepest box is bold. That is a glyph and a weight, not a
    color: the color channel already carries the profile, and the lit path
    crosses every row, so painting it would fight the gradient the whole way
    up. These are the same frames the call-stack pane marks.

    Stepping moves the mark, which is the point — the drawer stays open while
    [space] plays and you watch the run walk its own profile, with the heap
    still on screen above it. [Enter] does the reverse: it jumps the replay
    to the first step of whatever bar the keyboard is on.

    The tree is built once, over the whole trace, and does not change as the
    replay steps; only the mark moves. So a path — a zoom, a cursor — never
    goes stale the way a heap address does, and the app keeps both across a
    step, and across shutting the drawer and opening it again. *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** A bar's address: the keys from a root down to it, outermost first. [[]]
    is the whole tree — what an unzoomed drawer is zoomed to. Resolve one
    with {!Jsip_types.Flame_tree.find}; because the tree is static, a path
    stays valid for the whole session. *)
module Path : sig
  type t = Flame_tree.Key.t list [@@deriving sexp_of, equal]
end

(** Where [wasd] goes, SPATIALLY — the flames rise, so a caller's callees sit
    above it: [Up] goes deeper, into the callee that ran most, [Down] comes
    back toward the root, and [Left]/[Right] run along the callees one caller
    has. (The heap pane's [w] climbs to a parent for the same reason: its
    tree is drawn root-first, this one root-last.) *)
module Direction : sig
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, equal]
end

(** How one row's columns are divided among the box below it. [children] are
    the boxes actually drawn, in the order they are given; [pool] is the [+N]
    marker standing in for the [pooled] children that would have rounded to
    nothing; [self] is the gap left for the parent's own calls — its exposed
    top edge, where the self time is. The four always sum to the row's width
    — that is the whole contract, and {!columns} is the only place it is
    enforced. *)
module Columns : sig
  type t =
    { children : int list
    ; pool : int (** [0] when every child fit *)
    ; pooled : int (** how many children the [+N] stands for *)
    ; self : int
    }
  [@@deriving sexp_of, equal]
end

(** [columns ~width ~self ~children] apportions [width] cells among a node's
    children (by their [inclusive] weights, in the order given) and its own
    calls, so that [sum children + pool + self = width] exactly.

    Every drawn child gets at least one column, because a child rounded away
    is work made invisible; when there is not room for even that, the tail of
    the list pools into a single [+N] marker, floored at three columns so it
    can say how many it stands for rather than reading as one more box. The
    parent's own calls have no floor at all: an unlabelled gap that rounds
    away hides nothing, since a gap is what self time looks like anyway.

    Largest-remainder (Hamilton) apportionment, ties broken by draw order, so
    the result is a total function of the arguments and {!view}, {!bar_at}
    and {!move_cursor} cannot disagree about where a box begins.

    {v
      width 40, self 4, children 7 5 3 1  ->  14 10 6 3, self 7
      width  3, self 1, children 7 5 3 1  ->  1 1, pool 1 (pooled 2)
    v} *)
val columns : width:int -> self:int -> children:int list -> Columns.t

(** The path this step's frames trace through the tree — what {!view} draws
    the [▏] mark down. Outermost frame first, so it reads bottom-up like the
    picture. *)
val live_path : Flame_tree.t -> frames:Call.t list -> Path.t

(** Exactly [width * height] cells, through {!Panel.view}. [open_] only picks
    the title's [▾]/[▸]: a shut drawer is a region one row tall, so the body
    falls away on its own and no case is needed for it here.

    [zoom] is the box the drawer is scaled to: it spans the body's full width
    and everything above it is measured against it. [[]] — and any path that
    no longer resolves — means the whole tree, the roots sharing the bottom
    row. [cursor] is the box the keyboard is on, filled the orange every pane
    uses for "where [Enter] would go"; [None] before anything is aimed at. On
    a filled box a wash is the only cue available, so the cursor takes the
    whole box rather than riding on it. [depth_scroll] is the shallowest
    depth drawn, clamped so the cursor's row — or, with no cursor, the
    deepest lit row — stays on screen; the meta line says which depths are
    showing whenever they are not all of them. A tree shorter than the drawer
    is bottom-aligned: the root keeps the last row and the spare space sits
    above the flames.

    A box's fill is {!Jsip_types.Flame_tree.prorated_share} through
    {!Theme.flame}; one the profile has nothing to say about takes
    {!Theme.flame_neutral}, which is off the ramp, so "no data" cannot be
    misread as "cold". With no profile at all there is nothing to be neutral
    about, so every box is filled from its share of call volume instead and
    the meta line says [color = calls]. *)
val view
  :  width:int
  -> height:int
  -> open_:bool
  -> tree:Flame_tree.t
  -> live:Path.t
  -> zoom:Path.t
  -> cursor:Path.t option
  -> depth_scroll:int
  -> View.t

(** The bar a click at body position [(x, row)] lands on, mirroring {!view}'s
    apportionment and depth scrolling — the app jumps the replay to that
    bar's [first_step], which is what [Enter] on it does too. [None] over a
    parent's [·] self run (nothing deeper ran there), over a [+N] pool (its
    children are reachable only by zooming its parent), below the tree, and
    everywhere while the drawer is shut.

    Takes every argument {!view} takes, including the ones that do not change
    a bar's geometry, so the two cannot be called with different pictures in
    mind. *)
val bar_at
  :  width:int
  -> height:int
  -> tree:Flame_tree.t
  -> live:Path.t
  -> zoom:Path.t
  -> cursor:Path.t option
  -> depth_scroll:int
  -> x:int
  -> row:int
  -> Path.t option

(** Where [wasd] lands from the cursor — or, with no cursor, the deepest lit
    box inside the zoom, so the first keypress after [Tab] starts where the
    replay is. [Down] stops at the zoom's root row; [Up] takes the widest
    callee, which with siblings in name order is not simply the first one.
    [Left]/[Right] only reach boxes the apportionment actually drew — the
    ones behind a [+N] are not destinations, which is why this needs [width].
    [None] when nothing lies that way and the cursor should stay put. *)
val move_cursor
  :  width:int
  -> tree:Flame_tree.t
  -> zoom:Path.t
  -> cursor:Path.t option
  -> live:Path.t
  -> direction:Direction.t
  -> Path.t option
