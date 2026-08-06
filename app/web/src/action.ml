open! Core
open Jsip_types
open Jsip_web_components

module Lod = struct
  type t =
    | Uniform
    | Focal
  [@@deriving sexp_of, equal]

  let toggle t = match t with Uniform -> Focal | Focal -> Uniform

  let display t =
    match t with Uniform -> "uniform LOD" | Focal -> "focal LOD"
  ;;
end

(* The heap pane's two readings of the same step, as tabs. The canvas draws
   every structure as the tree of blocks it physically is; the outline
   ({!Jsip_web_components.Heap_outline}) names what each one HOLDS, one
   indented row per binding or element — the TUI pane's reading. Both are
   built from the same scene inputs, so the folds, the [/] filter, the [o]
   address order and the [z] accordion carry across the tab. *)
module Heap_view = struct
  type t =
    | Diagram
    | Outline
  [@@deriving sexp_of, equal]

  let toggle t = match t with Diagram -> Outline | Outline -> Diagram
  let display t = match t with Diagram -> "DIAGRAM" | Outline -> "OUTLINE"
end

module Edge_style = struct
  type t =
    | Angled
    | Orthogonal
    | Curved
  [@@deriving sexp_of, equal]

  let cycle t =
    match t with
    | Angled -> Orthogonal
    | Orthogonal -> Curved
    | Curved -> Angled
  ;;
end

(* The two marks the heap pane carries, and what [⌖] alternates between: the
   box you pinned (blue) and the structure this step walked (orange). One
   button rather than two, because they are the same question — "take me back
   to the thing I care about" — asked of whichever of them you last meant. *)
module Focus_target = struct
  type t =
    | Selection
    | Current
  [@@deriving sexp_of, equal]

  let toggle t = match t with Selection -> Current | Current -> Selection

  (* named for where a press GOES, not where it has been *)
  let display t =
    match t with Selection -> "⌖ pinned" | Current -> "⌖ latest"
  ;;
end

module Theme_mode = struct
  type t =
    | Dark
    | Light
  [@@deriving sexp_of, equal]

  let toggle t = match t with Dark -> Light | Light -> Dark
  let palette t = match t with Dark -> Theme.dark | Light -> Theme.light
end

(* a source fold is per file: the dump may span several *)
module Source_fold = struct
  module T = struct
    type t =
      { file : string
      ; line : int
      }
    [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

type t =
  | Step_to of int
  | Step_delta of int
  | Tick
  | Toggle_play
  | Select_frame of int
  | Frame_delta of int (** [↑]/[↓]: walk the live chain *)
  | Toggle_stack_fold of int
  | Toggle_stack_run of int
  | Toggle_source_fold of Source_fold.t
  | Toggle_stack_pane
  | Toggle_source_pane
  | Toggle_heap_fold of Heap_scene.Fold_key.t
  | Toggle_accordion
  | Toggle_address_order
  | Set_columns of int
  (** the heap's corner slider: how many structures stand side by side *)
  | Focus_latest
  | Begin_filter
  | Set_filter of string
  | Commit_filter
  | Cancel_filter
  | Select_heap_address of Snapshot.Address.t
  (** a click on a box: pin it blue and jump to its allocation step *)
  | Set_heap_view of Heap_view.t (** a click on the heap pane's tabs *)
  | Toggle_heap_view (** [v]: the same, from the keyboard *)
  | Toggle_lod
  | Cycle_edge_style
  | Toggle_theme (** [t]: light and dark, the whole surface at once *)
  | Toggle_flame
  | Jump_flame of Flame_math.Path.t
  (** a click on a bar: the replay to the first of the calls it merged *)
  | Zoom_flame of Flame_math.Path.t
  (** a double-click: the drawer rescaled to the bar *)
  | Reset_flame_zoom
  | Set_hud of
      { zoom_percent : int
      ; tier : float
      }
  | Quit (** [q]: close the tab, where the browser allows it *)
[@@deriving sexp_of]
