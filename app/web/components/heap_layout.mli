(** Geometry for the heap canvas: per-tier tree layouts and the semantic zoom
    that crossfades between them.

    Zoom is SEMANTIC: the wheel does not scale one picture, it moves a
    continuous detail tier [0. .. 3.] derived from the scale factor — postage
    stamp, label, fields, machine words — and each integer tier has its own
    layout, laid out for what its boxes hold at that detail. Geometry
    interpolates between the two neighbouring tiers' layouts ({!box_of}); the
    drawn content switches only once a box has nearly finished growing
    ({!content_tier}), so text never draws into a box that cannot hold it
    yet.

    All pure float math over a {!Heap_scene} — the canvas widget draws it,
    tests read it. Text is measured by character count (the canvas font is
    monospace), so sizes are deterministic. *)

open! Core

(** A {!Heap_scene.Fold_key} as the flat string the layout tables key on —
    ["12:0.3.1"]. *)
val key_id : Heap_scene.Fold_key.t -> string

module Box : sig
  type t =
    { x : float
    ; y : float
    ; w : float
    ; h : float
    }
  [@@deriving sexp_of, equal]

  val lerp : t -> t -> amount:float -> t
end

(** One box's place in one tier's layout, with what to draw in it. *)
module Placed : sig
  type t =
    { id : string
    ; node : Heap_scene.Node.t
    ; parent : string option
    ; edge_label : string
    ; depth : int
    }
  [@@deriving sexp_of]
end

module Head : sig
  type t =
    { root : Heap_scene.Root.t
    ; x : float (** the structure's left edge, in world coordinates *)
    ; y : float (** the header line's top *)
    }
end

module Tier_layout : sig
  type t =
    { placed : Placed.t list (** pre-order, parents before children *)
    ; pos : Box.t String.Map.t (** by {!key_id} *)
    ; heads : Head.t list
    ; width : float
    ; height : float
    }
end

(** A node's box size at a tier — exposed for the widget's focal (fisheye)
    mode, which grows single boxes past the global tier. *)
val size : Heap_scene.Node.t -> tier:int -> float * float

(** One tier's layout. [columns] is how many structures stand side by side
    before the next row of them — the canvas is much wider than one tree, so
    the structures pack into a grid rather than a single column. Each column
    is as wide as its widest structure and each row as tall as its tallest,
    and a column no structure landed in costs nothing. *)
val compute
  :  Heap_scene.Root.t list
  -> tier:int
  -> columns:int
  -> Tier_layout.t

(** All four tiers at once — what everything below interpolates over. *)
val all : Heap_scene.Root.t list -> columns:int -> Tier_layout.t array

(** The scale factors where each integer tier begins. *)
val stops : float array

(** Scale factor to continuous tier, log-interpolated between {!stops}. *)
val tier_for : k:float -> float

(** The inverse — what scale reaches a given tier. *)
val k_for_tier : float -> float

module Split : sig
  type t =
    { a : int (** the tier being left *)
    ; b : int (** the tier being approached *)
    ; f : float (** how far the GEOMETRY has crossed, [0. .. 1.] *)
    }
end

val split : float -> Split.t

(** The tier whose CONTENT a box draws at this continuous tier. *)
val content_tier : float -> int

(** The box for [id] at a continuous tier — the two neighbouring layouts'
    boxes crossfaded. [None] for a node not in the scene. *)
val box_of : Tier_layout.t array -> tier_f:float -> id:string -> Box.t option

val heads_now : Tier_layout.t array -> tier_f:float -> Head.t list

(** The whole canvas's extent at a continuous tier, [(width, height)]. *)
val bounds_now : Tier_layout.t array -> tier_f:float -> float * float
