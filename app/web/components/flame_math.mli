(** The flame drawer's bars, as data: the {!Jsip_types.Flame_tree} spread
    over a pixel width — the TUI {!Jsip_tui_components.Flame_pane}'s
    apportionment redone in floats, since a browser has no cell grid.

    Same contract: one row per call depth, roots at the BOTTOM, siblings in
    name order, a bar's width its subtree's share of the trace's events and
    its color its function's share of sampled compute (call volume when there
    is no profile). Children too narrow to draw pool into a [+N] marker
    rather than vanishing, and the gap at a row's right end is the parent's
    own calls — exposed self time, drawn as nothing. *)

open! Core
open Jsip_types

(** A bar's address — {!Jsip_types.Flame_tree.Key} list from a root down,
    outermost first; [[]] is the whole tree. *)
module Path : sig
  type t = Flame_tree.Key.t list [@@deriving sexp_of, equal]
end

module Segment : sig
  type t =
    | Bar of
        { path : Path.t
        ; label : string
        ; x : float
        ; width : float
        ; share : float option
        (** what to color by — [None] is "no data", the neutral fill *)
        ; lit : bool (** on the replay's live path *)
        ; deepest : bool (** the live path's innermost frame — bold *)
        }
    | Pool of
        { x : float
        ; width : float
        ; count : int
        }
  [@@deriving sexp_of]
end

module Row : sig
  type t =
    { depth : int (** 0 = the zoom root's row, drawn at the bottom *)
    ; segments : Segment.t list
    }
  [@@deriving sexp_of]
end

(** The path this step's frames trace through the tree — what lights up. *)
val live_path : Flame_tree.t -> frames:Call.t list -> Path.t

(** Every row of the drawer at this pixel [width], shallowest first. [zoom]
    scales the drawer to one bar ([[]], or a path that no longer resolves,
    means the whole tree). *)
val bars
  :  Flame_tree.t
  -> zoom:Path.t
  -> width:float
  -> live:Path.t
  -> Row.t list

(** Which measurement the colors carry — [`Compute] with a loaded profile,
    [`Calls] otherwise. The meta line's disclosure. *)
val heat_source : Flame_tree.t -> [ `Compute | `Calls ]
