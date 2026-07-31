open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Row = struct
  type t =
    { view : View.t
    ; address : Snapshot.Address.t option
    }
end

(* real addresses are twelve hex digits of noise; the box keeps the tail as
   an identity hint and the full address stays in the data *)
let short_address address =
  let hex = Snapshot.Address.display address in
  match String.length hex > 8 with
  | true -> [%string "0x…%{String.suffix hex 4}"]
  | false -> hex
;;

(* queue cells label their fields numerically on the wire (0 = content, 1 =
   next); everything else keeps its own label *)
let display_label ~ds_type label =
  match (ds_type : Snapshot.Ds_type.t), label with
  | Queue, "0" -> "v"
  | Queue, "1" -> "next"
  | (Map | Set | Queue), label -> label
;;

(* what the box says: the mockup's ["key" ↦ data] for map nodes, the element
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

(* the mockup's node card, in cells:
   {v
   ┌────────────────┐
   │ "a" ↦ 2    new │
   │ 0x…a278        │
   └────────────────┘
   v} *)
let box_rows
  (node : Snapshot.Node.t)
  ~ds_type
  ~hidden_labels
  ~new_addresses
  ~prefix
  ~rest
  =
  let is_new = Set.mem new_addresses node.virtual_address in
  let border_color =
    match is_new with true -> Theme.fresh | false -> Theme.border_strong
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
      (short_address node.virtual_address)
  in
  let inner = max (View.width summary) (View.width address) in
  let ghost_prefix text = View.text ~attrs:(Theme.fg' Theme.ghost) text in
  let horizontal = Panel.repeat "─" ~width:(inner + 2) in
  let content line =
    View.hcat
      [ ghost_prefix rest
      ; View.text ~attrs:border "│ "
      ; Panel.fit line ~width:inner ~height:1
      ; View.text ~attrs:border " │"
      ]
  in
  let row view = { Row.view; address = Some node.virtual_address } in
  [ row
      (View.hcat
         [ ghost_prefix prefix
         ; View.text ~attrs:border [%string "┌%{horizontal}┐"]
         ])
  ; row (content summary)
  ; row (content address)
  ; row
      (View.hcat
         [ ghost_prefix rest
         ; View.text ~attrs:border [%string "└%{horizontal}┘"]
         ])
  ]
;;

(* The walked tree, mockup-style: one card per node, pointer slots as labeled
   edges hanging below it. Which slots edge off is the emitter's
   masked-layout contract ({!Snapshot.Ds_type.masked_labels}): a missing
   masked label takes the next walked child, in order; an empty pointer shows
   as [∅] — but a leaf (no walked children at all) keeps its empty slots to
   itself. *)
let rec node_rows
  (node : Snapshot.Node.t)
  ~ds_type
  ~new_addresses
  ~prefix
  ~rest
  =
  let masked = Snapshot.Ds_type.masked_labels ds_type ~block:node.block in
  let nil_labels = Snapshot.Ds_type.nil_labels ds_type in
  let in_block label = List.Assoc.mem node.block label ~equal:String.equal in
  (* pointer slots never print as fields: a missing masked label is a walked
     child, and a masked nil ([Int 0] in a nil-able slot) is an empty pointer
     — drawn as an [∅] edge on interior nodes, and simply omitted on leaves
     so they stay compact *)
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
    box_rows
      node
      ~ds_type
      ~hidden_labels:pointer_labels
      ~new_addresses
      ~prefix
      ~rest
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
  let edges = labeled_edges @ unclaimed_children in
  let last_index = List.length edges - 1 in
  box
  @ List.concat_mapi edges ~f:(fun index (label, edge) ->
    let is_last = index = last_index in
    let connector = match is_last with true -> "└─" | false -> "├─" in
    let label = display_label ~ds_type label in
    let arrow =
      match label with "" -> "→ " | label -> [%string "%{label}→ "]
    in
    let head = rest ^ connector ^ arrow in
    match edge with
    | `Nil ->
      [ { Row.view =
            View.hcat
              [ View.text ~attrs:(Theme.fg' Theme.ghost) head
              ; View.text ~attrs:(Theme.fg' Theme.faint) "∅"
              ]
        ; address = None
        }
      ]
    | `Child child ->
      (* the arrow is [label ^ "→ "]: label chars + 2 display columns *)
      let arrow_columns = String.length label + 2 in
      let pad_under =
        rest
        ^ (match is_last with true -> "  " | false -> "│ ")
        ^ String.make arrow_columns ' '
      in
      node_rows child ~ds_type ~new_addresses ~prefix:head ~rest:pad_under)
;;

let rows ~snapshot ~new_addresses =
  node_rows
    snapshot.Snapshot.root_node
    ~ds_type:snapshot.ds_type
    ~new_addresses
    ~prefix:""
    ~rest:""
;;

let count_nodes snapshot =
  let rec count (node : Snapshot.Node.t) =
    1 + List.sum (module Int) node.children ~f:count
  in
  count snapshot.Snapshot.root_node
;;

let scroll_limit rows ~height = Int.max 0 (List.length rows - height)

let view ~width ~height ~snapshot ~new_addresses ~scroll =
  let all_rows = rows ~snapshot ~new_addresses in
  let scroll = Int.min scroll (scroll_limit all_rows ~height:(height - 2)) in
  let visible =
    List.drop all_rows scroll
    |> List.map ~f:(fun (row : Row.t) ->
      Panel.fit row.view ~width:(Panel.inner_width ~width) ~height:1)
  in
  let nodes = count_nodes snapshot in
  let fresh = Set.length new_addresses in
  let meta =
    let base =
      [%string
        "%{Snapshot.Ds_type.display snapshot.ds_type} · %{nodes#Int} nodes"]
    in
    match fresh with
    | 0 -> base
    | fresh -> [%string "%{base} · %{fresh#Int} new"]
  in
  Panel.view ~title:"heap" ~meta ~width ~height (View.vcat visible)
;;

let address_at ~snapshot ~new_addresses ~scroll ~height ~row =
  let all_rows = rows ~snapshot ~new_addresses in
  let scroll = Int.min scroll (scroll_limit all_rows ~height:(height - 2)) in
  match List.nth all_rows (row + scroll) with
  | Some { Row.address; view = _ } -> address
  | None -> None
;;
