(** The heap pane's overview: one tile per live structure, no scrolling.

    {!Heap_pane} draws every structure's tree, which stops being readable
    long before a real program stops allocating — an exchange scenario ends
    with hundreds of live structures and a canvas many screens tall, and
    scrolling a diagram is a poor way to find anything in it. When that
    canvas no longer fits the pane, the app draws this instead: a grid of
    tiles, one per structure, sized so the whole registry is on screen at
    once. Clicking a tile opens that structure alone on the ordinary canvas
    — where it gets the whole pane and the centering — and [esc] (or the
    transport's [esc close] chip) comes back here.

    Tiles take the roomiest shape that fits: a small card carrying the
    structure's name, kind and node count, or — once there are too many for
    that — one line each. When even one line each overflows, the last cell
    reads [+n more] rather than quietly dropping the tail; [/] is how you
    narrow the registry down to what you are looking for.

    {v
    ┌ orders ────┐  ┌ book ──────┐  ┌ tape ──────┐
    │hashtbl     │  │map         │  │fdeque      │
    │412 nodes   │  │88 nodes    │  │30 nodes    │
    └────────────┘  └────────────┘  └────────────┘
    v} *)

open! Core
open Jsip_replay
module View := Bonsai_term.View

(** The grid, framed as the heap pane. [structures] is the live registry
    after any [/] filter, in registry order, and [total] the count before
    it — the header owns up to the cut the way {!Heap_pane}'s does. [note]
    rides ahead of the counts for an app-level mode (the live filter, the
    accordion light). *)
val view
  :  note:string option
  -> total:int option
  -> width:int
  -> height:int
  -> structures:Replay.Structure.t list
  -> View.t

(** The structure a click at pane-body position [(x, y)] opens, mirroring
    [view]'s grid. [None] on the gaps between tiles and on the [+n more]
    cell, which names no one structure. *)
val structure_at
  :  width:int
  -> height:int
  -> structures:Replay.Structure.t list
  -> x:int
  -> y:int
  -> int option
