open! Core
open Jsip_types
open Jsip_replay
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

let gap = 2
let row_gap = 1

(* past this a tile stops growing and its text is elided: one structure with
   a long name should not set the column width for all of them *)
let max_tile_width = 26

(* mirrors the count on {!Heap_pane}'s section headers — the same number, so
   a tile and the canvas it opens into never disagree *)
let node_count (structure : Replay.Structure.t) =
  Snapshot.Node.fold
    structure.snapshot.root_node
    ~init:0
    ~f:(fun n (_ : Snapshot.Node.t) -> n + 1)
;;

let size_text structure =
  match node_count structure with
  | 1 -> "1 node"
  | count -> [%string "%{count#Int} nodes"]
;;

let name_text (structure : Replay.Structure.t) =
  Replay.Structure.display structure
;;

let kind_text (structure : Replay.Structure.t) =
  Snapshot.Ds_type.display structure.snapshot.ds_type
;;

(* Names and kinds are program identifiers, so a byte prefix is a character
   prefix. Anything carrying a multi-byte glyph is left alone and cropped by
   the tile instead, rather than sliced mid-character. *)
let elide text ~width =
  match View.width (View.text text) <= width with
  | true -> text
  | false ->
    (match String.for_all text ~f:(fun c -> Char.to_int c < 128) with
     | false -> text
     | true -> String.prefix text (Int.max 0 (width - 1)) ^ "…")
;;

module Shape = struct
  (* How much of a structure a tile shows. The grid takes the roomiest one
     that fits the pane, so a handful of structures get cards and a hundred
     get one line each. *)
  type t =
    | Card (** name on the border, kind and size inside *)
    | Line (** [name · kind · n nodes] on one row *)

  let height = function
    | Card -> 4
    | Line -> 1
  ;;

  let line_text structure =
    [%string
      "%{name_text structure} · %{kind_text structure} · %{size_text \
       structure}"]
  ;;

  (* what one structure wants, before the grid settles on a common width *)
  let width t structure =
    let columns text = View.width (View.text text) in
    let text =
      match t with
      | Card ->
        List.map
          [ name_text structure; kind_text structure; size_text structure ]
          ~f:columns
        |> List.max_elt ~compare:Int.compare
        |> Option.value ~default:0
      | Line -> columns (line_text structure)
    in
    Int.min max_tile_width (text + 4)
  ;;
end

module Cell = struct
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    ; structure : Replay.Structure.t
    }

  let contains t ~x ~y =
    x >= t.x && x < t.x + t.width && y >= t.y && y < t.y + t.height
  ;;
end

(* the grid, fully decided: [view] draws it and [structure_at] reads it, so
   a click cannot land anywhere the eye was not looking *)
module Plan = struct
  type t =
    { shape : Shape.t
    ; cells : Cell.t list
    ; hidden : int (** structures the grid had no room for *)
    ; tile_width : int
    ; hidden_note : (int * int) option (** where [+n more] goes *)
    }
end

let tile_width shape ~structures =
  List.map structures ~f:(Shape.width shape)
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:1
;;

let columns_of ~width ~tile_width =
  Int.max 1 ((width + gap) / (tile_width + gap))
;;

let block_left ~width ~columns ~tile_width =
  let block =
    Int.min width (Int.max 0 ((columns * (tile_width + gap)) - gap))
  in
  Int.max 0 ((width - block) / 2)
;;

(* The grid for one shape: as many columns as fit, rows filled left to
   right, and the block centered in the pane the way the detail canvas
   centers its structures. [None] when the rows overflow — the caller walks
   the shapes roomiest first and takes the first that holds everything. *)
let grid shape ~width ~height ~structures =
  let count = List.length structures in
  let tile_width = tile_width shape ~structures in
  let tile_height = Shape.height shape in
  let columns = columns_of ~width ~tile_width in
  let rows = (count + columns - 1) / columns in
  let block_height = Int.max 0 ((rows * (tile_height + row_gap)) - row_gap) in
  let left = block_left ~width ~columns ~tile_width in
  let top = Int.max 0 ((height - block_height) / 2) in
  let cells =
    List.mapi structures ~f:(fun index structure ->
      { Cell.x = left + (index % columns * (tile_width + gap))
      ; y = top + (index / columns * (tile_height + row_gap))
      ; width = tile_width
      ; height = tile_height
      ; structure
      })
  in
  match block_height <= height with
  | true ->
    Some { Plan.shape; cells; hidden = 0; tile_width; hidden_note = None }
  | false -> None
;;

(* Not even one line each fits, so the last cell that does says how many
   structures are not on screen — the same admission the heap header makes
   under a filter, and [/] is how you go and find them. *)
let truncated_grid ~width ~height ~structures =
  let shape = Shape.Line in
  let tile_width = tile_width shape ~structures in
  let tile_height = Shape.height shape in
  let columns = columns_of ~width ~tile_width in
  let rows = Int.max 1 ((height + row_gap) / (tile_height + row_gap)) in
  let capacity = Int.max 1 ((columns * rows) - 1) in
  let shown = List.take structures capacity in
  let left = block_left ~width ~columns ~tile_width in
  let spot index =
    ( left + (index % columns * (tile_width + gap))
    , index / columns * (tile_height + row_gap) )
  in
  let cells =
    List.mapi shown ~f:(fun index structure ->
      let x, y = spot index in
      { Cell.x; y; width = tile_width; height = tile_height; structure })
  in
  { Plan.shape
  ; cells
  ; hidden = List.length structures - List.length shown
  ; tile_width
  ; hidden_note = Some (spot (List.length shown))
  }
;;

let plan ~width ~height ~structures =
  let width = Int.max 1 width in
  let height = Int.max 1 height in
  match structures with
  | [] ->
    { Plan.shape = Shape.Line
    ; cells = []
    ; hidden = 0
    ; tile_width = 1
    ; hidden_note = None
    }
  | _ :: _ ->
    (match
       List.find_map [ Shape.Card; Shape.Line ] ~f:(fun shape ->
         grid shape ~width ~height ~structures)
     with
     | Some plan -> plan
     | None -> truncated_grid ~width ~height ~structures)
;;

(* the structure this step's event walked reads in the highlight blue, the
   way its header does on the detail canvas *)
let tile_attrs (structure : Replay.Structure.t) =
  match structure.is_current with
  | true -> [ Theme.fg Theme.highlight_deep; Attr.bold ], Theme.highlight
  | false -> [ Theme.fg Theme.text ], Theme.card_border
;;

let inner_row text ~attrs ~width ~border =
  View.hcat
    [ View.text ~attrs:(Theme.fg' border) "│"
    ; Panel.fit (View.text ~attrs (elide text ~width)) ~width ~height:1
    ; View.text ~attrs:(Theme.fg' border) "│"
    ]
;;

(* the structure's name rides the top border, the way a card's does on the
   detail canvas, so a tile reads as the same object drawn smaller *)
let card_tile structure ~width =
  let label_attrs, border = tile_attrs structure in
  let inner = Int.max 1 (width - 2) in
  let name = elide (name_text structure) ~width:(Int.max 1 (inner - 2)) in
  let rule =
    Panel.repeat
      "─"
      ~width:(Int.max 0 (inner - View.width (View.text name) - 2))
  in
  View.vcat
    [ View.hcat
        [ View.text ~attrs:(Theme.fg' border) "┌ "
        ; View.text ~attrs:label_attrs name
        ; View.text ~attrs:(Theme.fg' border) [%string " %{rule}┐"]
        ]
    ; inner_row
        (kind_text structure)
        ~attrs:(Theme.fg' Theme.secondary)
        ~width:inner
        ~border
    ; inner_row
        (size_text structure)
        ~attrs:(Theme.fg' Theme.muted)
        ~width:inner
        ~border
    ; View.text
        ~attrs:(Theme.fg' border)
        [%string "└%{Panel.repeat \"─\" ~width:inner}┘"]
    ]
;;

let line_tile structure ~width =
  let label_attrs, (_ : Attr.Color.t) = tile_attrs structure in
  Panel.fit
    (View.text ~attrs:label_attrs (elide (Shape.line_text structure) ~width))
    ~width
    ~height:1
;;

let tile (plan : Plan.t) structure =
  match plan.shape with
  | Shape.Card -> card_tile structure ~width:plan.tile_width
  | Shape.Line -> line_tile structure ~width:plan.tile_width
;;

let view ~note ~total ~width ~height ~structures =
  let plan =
    plan
      ~width:(Panel.inner_width ~width)
      ~height:(height - Panel.header_height)
      ~structures
  in
  let tiles =
    List.map plan.cells ~f:(fun (cell : Cell.t) ->
      View.pad ~l:cell.x ~t:cell.y (tile plan cell.structure))
  in
  let hidden =
    match plan.hidden_note with
    | None -> []
    | Some (x, y) ->
      [ View.pad
          ~l:x
          ~t:y
          (View.text
             ~attrs:(Theme.fg' Theme.accent)
             [%string "+%{plan.hidden#Int} more"])
      ]
  in
  let live = List.length structures in
  let meta =
    let living =
      match total with
      | Some total when total <> live ->
        [%string "%{live#Int} of %{total#Int} live"]
      | Some (_ : int) | None -> [%string "%{live#Int} live"]
    in
    let base = [%string "%{living} · click one to open it"] in
    match note with
    | None -> base
    | Some note -> [%string "%{note} · %{base}"]
  in
  Panel.view ~title:"heap" ~meta ~width ~height (View.zcat (tiles @ hidden))
;;

let structure_at ~width ~height ~structures ~x ~y =
  let plan =
    plan
      ~width:(Panel.inner_width ~width)
      ~height:(height - Panel.header_height)
      ~structures
  in
  List.find plan.cells ~f:(fun (cell : Cell.t) -> Cell.contains cell ~x ~y)
  |> Option.map ~f:(fun (cell : Cell.t) -> cell.structure.Replay.Structure.id)
;;
