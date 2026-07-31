open! Core
open Jsip_types
open Jsip_replay
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

(* where a node's card landed on the tree canvas, for click-to-jump *)
module Placed = struct
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    ; address : Snapshot.Address.t
    }

  let contains t ~x ~y =
    x >= t.x && x < t.x + t.width && y >= t.y && y < t.y + t.height
  ;;

  let shift t ~dx ~dy = { t with x = t.x + dx; y = t.y + dy }
end

(* queue cells label their fields numerically on the wire (0 = content, 1 =
   next); everything else keeps its own label *)
let display_label ~ds_type label =
  match (ds_type : Snapshot.Ds_type.t), label with
  | Queue, "0" -> "v"
  | Queue, "1" -> "next"
  | (Map | Set | Queue), label -> label
;;

(* what the card says: the mockup's ["key" ↦ data] for map nodes, the element
   for sets, [length n] for queue roots, the content for cells, the joined
   positions for a walked value block — plus any leftover fields, labeled *)
let summary_spans (node : Snapshot.Node.t) ~ds_type ~hidden_labels =
  let field label =
    List.Assoc.find node.block label ~equal:String.equal
    |> Option.map ~f:Snapshot.Block.display
  in
  let used, main =
    match (ds_type : Snapshot.Ds_type.t) with
    | (Map | Set) when Snapshot.Ds_type.is_value_block node.block ->
      ( List.map node.block ~f:fst
      , [ ( `Value
          , List.map node.block ~f:(fun ((_ : string), block) ->
              Snapshot.Block.display block)
            |> String.concat ~sep:", " )
        ] )
    | Map ->
      ( [ "v"; "d" ]
      , List.concat
          [ (match field "v" with Some key -> [ `Key, key ] | None -> [])
          ; [ `Arrow, " ↦ " ]
          ; (match field "d" with
             | Some data -> [ `Value, data ]
             | None -> [])
          ] )
    | Set -> [ "v" ], [ `Key, Option.value (field "v") ~default:"·" ]
    | Queue ->
      (match field "length" with
       | Some length -> [ "length" ], [ `Label, "length "; `Value, length ]
       | None -> [ "0" ], [ `Value, Option.value (field "0") ~default:"·" ])
  in
  let leftovers =
    List.filter node.block ~f:(fun (label, (_ : Snapshot.Block.t)) ->
      (not (List.mem hidden_labels label ~equal:String.equal))
      && not (List.mem used label ~equal:String.equal))
    |> List.concat_map ~f:(fun (label, block) ->
      [ `Label, [%string "  %{display_label ~ds_type label}="]
      ; `Value, Snapshot.Block.display block
      ])
  in
  main @ leftovers
;;

let span_view (tag, text) =
  let attrs =
    match tag with
    | `Key -> [ Theme.fg Theme.text; Attr.bold ]
    | `Value -> Theme.fg' Theme.text
    | `Arrow -> Theme.fg' Theme.ghost
    | `Label -> Theme.fg' Theme.muted
  in
  View.text ~attrs text
;;

(* the mockup's node card — gold outline, the full address in small type:
   {v
   ┌──────────────────────┐
   │ "a" ↦ 2          new │
   │ 0x763be65ee878       │
   └──────────────────────┘
   v} *)
let node_box (node : Snapshot.Node.t) ~ds_type ~hidden_labels ~new_addresses =
  let is_new = Set.mem new_addresses node.virtual_address in
  let border_color =
    match is_new with true -> Theme.fresh | false -> Theme.accent
  in
  let border = Theme.fg' border_color in
  let summary =
    let spans =
      List.map (summary_spans node ~ds_type ~hidden_labels) ~f:span_view
    in
    match is_new with
    | true ->
      View.hcat
        (spans
         @ [ View.text "  "
           ; View.text ~attrs:[ Theme.fg Theme.fresh; Attr.italic ] "new"
           ])
    | false -> View.hcat spans
  in
  let address =
    View.text
      ~attrs:(Theme.fg' Theme.faint)
      (Snapshot.Address.display node.virtual_address)
  in
  let inner = max (View.width summary) (View.width address) in
  let horizontal = Panel.repeat "─" ~width:(inner + 2) in
  let content line =
    View.hcat
      [ View.text ~attrs:border "│ "
      ; Panel.fit line ~width:inner ~height:1
      ; View.text ~attrs:border " │"
      ]
  in
  View.vcat
    [ View.text ~attrs:border [%string "┌%{horizontal}┐"]
    ; content summary
    ; content address
    ; View.text ~attrs:border [%string "└%{horizontal}┘"]
    ]
;;

let sibling_gap = 3

(* the ┌──┴──┐ rail between a parent and its children, hooks at each child's
   center *)
let rail ~parent_center ~centers =
  let leftmost = List.min_elt centers ~compare |> Option.value ~default:0 in
  let rightmost = List.max_elt centers ~compare |> Option.value ~default:0 in
  let glyph x =
    let is_child = List.mem centers x ~equal:Int.equal in
    let is_parent = x = parent_center in
    match x < leftmost || x > rightmost with
    | true -> " "
    | false ->
      (match is_parent, is_child with
       | true, true ->
         (* a lone child hangs straight down; an aligned middle child crosses
            the rail *)
         (match leftmost = rightmost with true -> "│" | false -> "┼")
       | true, false -> "┴"
       | false, true ->
         (match x = leftmost, x = rightmost with
          | true, _ -> "┌"
          | _, true -> "┐"
          | false, false -> "┬")
       | false, false -> "─")
  in
  View.text
    ~attrs:(Theme.fg' Theme.ghost)
    (String.concat (List.init (rightmost + 1) ~f:glyph))
;;

(* edge labels sitting under their hooks *)
let rail_labels ~labeled_centers =
  let width =
    List.fold labeled_centers ~init:0 ~f:(fun width (center, label) ->
      max width (center + 1 + (String.length label / 2) + String.length label))
  in
  let buffer = Bytes.make width ' ' in
  List.iter labeled_centers ~f:(fun (center, label) ->
    let start = max 0 (center - (String.length label / 2)) in
    String.iteri label ~f:(fun i char ->
      let at = start + i in
      match at < width with true -> Bytes.set buffer at char | false -> ()));
  View.text
    ~attrs:(Theme.fg' Theme.muted)
    (Bytes.to_string buffer |> String.rstrip)
;;

(* Lay the subtree out the way a CS diagram draws it: the node's card
   centered over its children, siblings side by side on one level, a rail
   connecting the card to each child's center. Returns the canvas, the card's
   center column, and every card's position for hit-testing. *)
let rec tree (node : Snapshot.Node.t) ~ds_type ~new_addresses
  : View.t * int * Placed.t list
  =
  let masked = Snapshot.Ds_type.masked_labels ds_type ~block:node.block in
  let nil_labels = Snapshot.Ds_type.nil_labels ds_type in
  let in_block label = List.Assoc.mem node.block label ~equal:String.equal in
  (* pointer slots never print as fields: a missing masked label is a walked
     child, and a masked nil is an empty pointer — shown as [∅] under
     interior nodes, omitted under leaves *)
  let empty_pointer label =
    List.mem nil_labels label ~equal:String.equal
    &&
    match List.Assoc.find node.block label ~equal:String.equal with
    | Some (Snapshot.Block.Int 0) -> true
    | Some _ | None -> false
  in
  let pointer_labels =
    List.filter masked ~f:(fun label ->
      (not (in_block label)) || empty_pointer label)
  in
  let edge_labels =
    match List.is_empty node.children with
    | true ->
      List.filter pointer_labels ~f:(fun label -> not (in_block label))
    | false -> pointer_labels
  in
  let box =
    node_box node ~ds_type ~hidden_labels:pointer_labels ~new_addresses
  in
  let box_width = View.width box in
  let box_height = View.height box in
  let box_placed =
    { Placed.x = 0
    ; y = 0
    ; width = box_width
    ; height = box_height
    ; address = node.virtual_address
    }
  in
  let children = Queue.of_list node.children in
  (* bind the labeled edges before flushing the queue: (@) evaluates its
     arguments right to left, and the dequeues must come first *)
  let labeled_edges =
    List.map edge_labels ~f:(fun label ->
      match in_block label with
      | true -> label, `Nil
      | false ->
        (match Queue.dequeue children with
         | Some child -> label, `Child child
         | None -> label, `Nil))
  in
  let unclaimed_children =
    List.map (Queue.to_list children) ~f:(fun child -> "", `Child child)
  in
  match labeled_edges @ unclaimed_children with
  | [] -> box, box_width / 2, [ box_placed ]
  | edges ->
    let rendered =
      List.map edges ~f:(fun (label, edge) ->
        let label = display_label ~ds_type label in
        match edge with
        | `Nil -> label, (View.text ~attrs:(Theme.fg' Theme.faint) "∅", 0, [])
        | `Child child -> label, tree child ~ds_type ~new_addresses)
    in
    (* children row, left to right *)
    let (_ : int), placed_children =
      List.fold_map
        rendered
        ~init:0
        ~f:(fun x (label, (view, center, placed)) ->
          ( x + View.width view + sibling_gap
          , (label, view, x, x + center, placed) ))
    in
    let centers =
      List.map
        placed_children
        ~f:
          (fun
            ( (_ : string)
            , (_ : View.t)
            , (_ : int)
            , center
            , (_ : Placed.t list) )
          -> center)
    in
    let leftmost = List.hd_exn centers in
    let rightmost = List.last_exn centers in
    let midpoint = (leftmost + rightmost) / 2 in
    (* center the card over its children; if the card is wider than the
       spread, shift the children right instead *)
    let parent_x = max 0 (midpoint - (box_width / 2)) in
    let child_shift = max 0 ((box_width / 2) - midpoint) in
    let centers = List.map centers ~f:(fun center -> center + child_shift) in
    let parent_center = parent_x + (box_width / 2) in
    let labeled_centers =
      List.zip_exn
        centers
        (List.map
           placed_children
           ~f:
             (fun
               ( label
               , (_ : View.t)
               , (_ : int)
               , (_ : int)
               , (_ : Placed.t list) )
             -> label))
      |> List.filter_map ~f:(fun (center, label) ->
        match String.is_empty label with
        | true -> None
        | false -> Some (center, label))
    in
    let rail_rows =
      [ rail ~parent_center ~centers ]
      @
      match List.is_empty labeled_centers with
      | true -> []
      | false -> [ rail_labels ~labeled_centers ]
    in
    let children_y = box_height + List.length rail_rows in
    let children_views, children_placed =
      List.fold
        placed_children
        ~init:([], [])
        ~f:
          (fun
            (views, all_placed) ((_ : string), view, x, (_ : int), placed) ->
          let x = x + child_shift in
          ( View.pad ~l:x ~t:children_y view :: views
          , List.map placed ~f:(Placed.shift ~dx:x ~dy:children_y)
            @ all_placed ))
    in
    let canvas =
      View.zcat
        ((View.pad ~l:parent_x box
          :: List.mapi rail_rows ~f:(fun i row ->
            View.pad ~t:(box_height + i) row))
         @ children_views)
    in
    ( canvas
    , parent_center
    , Placed.shift box_placed ~dx:parent_x ~dy:0 :: children_placed )
;;

(* the section header over one structure's tree: its registry id and kind,
   the one this step's event walked marked in the highlight blue *)
let structure_header { Replay.Structure.id; snapshot; is_current; _ } =
  let label =
    [%string "#%{id#Int} · %{Snapshot.Ds_type.display snapshot.ds_type}"]
  in
  match is_current with
  | true ->
    View.hcat
      [ View.text ~attrs:(Theme.fg' Theme.highlight) "▸ "
      ; View.text ~attrs:[ Theme.fg Theme.highlight_deep; Attr.bold ] label
      ]
  | false ->
    View.hcat
      [ View.text "  "; View.text ~attrs:(Theme.fg' Theme.muted) label ]
;;

(* every live structure, stacked: a header, its tree, a breathing row *)
let layout ~structures ~new_addresses =
  let views, placed, (_ : int) =
    List.fold
      structures
      ~init:([], [], 0)
      ~f:(fun (views, all_placed, y) (structure : Replay.Structure.t) ->
        let canvas, (_ : int), placed =
          tree
            structure.snapshot.root_node
            ~ds_type:structure.snapshot.ds_type
            ~new_addresses
        in
        let views =
          View.pad ~t:(y + 1) canvas
          :: View.pad ~t:y (structure_header structure)
          :: views
        in
        let placed =
          List.map placed ~f:(Placed.shift ~dx:0 ~dy:(y + 1)) @ all_placed
        in
        views, placed, y + 1 + View.height canvas + 1)
  in
  View.zcat views, placed
;;

let count_nodes structures =
  let rec count (node : Snapshot.Node.t) =
    1 + List.sum (module Int) node.children ~f:count
  in
  List.sum
    (module Int)
    structures
    ~f:(fun (structure : Replay.Structure.t) ->
      count structure.snapshot.root_node)
;;

let clamp_scroll canvas ~height ~scroll =
  Int.min scroll (Int.max 0 (View.height canvas - (height - 2)))
;;

let view ~width ~height ~structures ~new_addresses ~scroll =
  let canvas, (_ : Placed.t list) = layout ~structures ~new_addresses in
  let scroll = clamp_scroll canvas ~height ~scroll in
  let fresh = Set.length new_addresses in
  let live = List.length structures in
  let nodes = count_nodes structures in
  let meta =
    let base = [%string "%{live#Int} live · %{nodes#Int} nodes"] in
    match fresh with
    | 0 -> base
    | fresh -> [%string "%{base} · %{fresh#Int} new"]
  in
  Panel.view ~title:"heap" ~meta ~width ~height (View.crop ~t:scroll canvas)
;;

let address_at ~structures ~new_addresses ~scroll ~height ~x ~y =
  let canvas, placed = layout ~structures ~new_addresses in
  let scroll = clamp_scroll canvas ~height ~scroll in
  List.find placed ~f:(Placed.contains ~x ~y:(y + scroll))
  |> Option.map ~f:(fun (placed : Placed.t) -> placed.address)
;;
