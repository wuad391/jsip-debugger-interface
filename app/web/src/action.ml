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
