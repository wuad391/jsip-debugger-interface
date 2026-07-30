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

(* queue cells label their fields numerically on the wire (0 = content, 1 =
   next); everything else keeps its own label *)
let display_label ~ds_type label =
  match (ds_type : Snapshot.Ds_type.t), label with
  | Queue, "0" -> "v"
  | Queue, "1" -> "next"
  | (Map | Set | Queue), label -> label
;;

let node_line
  (node : Snapshot.Node.t)
  ~ds_type
  ~edge_labels
  ~new_addresses
  ~prefix
  =
  let is_new = Set.mem new_addresses node.virtual_address in
  let address_attrs =
    match is_new with
    | true -> [ Theme.fg Theme.fresh; Attr.bold ]
    | false -> Theme.fg' Theme.faint
  in
  let bullet_color =
    match is_new with true -> Theme.fresh | false -> Theme.accent
  in
  let fields =
    List.filter node.block ~f:(fun (label, _) ->
      not (List.mem edge_labels label ~equal:String.equal))
    |> List.concat_map ~f:(fun (label, block) ->
      [ View.text "  "
      ; View.text
          ~attrs:(Theme.fg' Theme.muted)
          [%string "%{display_label ~ds_type label}="]
      ; View.text
          ~attrs:(Theme.fg' Theme.text)
          (Snapshot.Block.display block)
      ])
  in
  let chip =
    match is_new with
    | true ->
      [ View.text "  "
      ; View.text ~attrs:[ Theme.fg Theme.fresh; Attr.italic ] "new"
      ]
    | false -> []
  in
  View.hcat
    ([ View.text ~attrs:(Theme.fg' Theme.ghost) prefix
     ; View.text ~attrs:[ Theme.fg bullet_color ] "● "
     ; View.text
         ~attrs:address_attrs
         (Snapshot.Address.display node.virtual_address)
     ]
     @ fields
     @ chip)
;;

(* The walked tree, mockup-style: value fields inline in the node line and
   pointer slots as labeled edges. Which slots edge off is the emitter's
   masked-layout contract ({!Snapshot.Ds_type.masked_labels}): a masked label
   present in [block] as an empty pointer draws as [∅], and each missing
   label takes the next walked child, in order. *)
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
  let edge_labels =
    (* a masked label that is either a walked child (absent) or an empty
       pointer draws as an edge; other present labels stay inline *)
    List.filter masked ~f:(fun label ->
      (not (in_block label))
      || (List.mem nil_labels label ~equal:String.equal
          &&
          match List.Assoc.find node.block label ~equal:String.equal with
          | Some (Snapshot.Block.Int 0) -> true
          | Some _ | None -> false))
  in
  let first =
    { Row.view = node_line node ~ds_type ~edge_labels ~new_addresses ~prefix
    ; address = Some node.virtual_address
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
  let edges = labeled_edges @ unclaimed_children in
  let last_index = List.length edges - 1 in
  first
  :: List.concat_mapi edges ~f:(fun index (label, edge) ->
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

let registry_row registry =
  let entries =
    List.concat_map registry ~f:(fun (id, address) ->
      [ View.text "  "
      ; View.text ~attrs:(Theme.fg' Theme.secondary) [%string "%{id#Int}"]
      ; View.text ~attrs:(Theme.fg' Theme.ghost) "↦"
      ; View.text
          ~attrs:(Theme.fg' Theme.faint)
          (Snapshot.Address.display address)
      ])
  in
  { Row.view =
      View.hcat (View.text ~attrs:(Theme.fg' Theme.muted) "live" :: entries)
  ; address = None
  }
;;

let rows ~snapshot ~registry ~new_addresses =
  let tree =
    node_rows
      snapshot.Snapshot.root_node
      ~ds_type:snapshot.ds_type
      ~new_addresses
      ~prefix:""
      ~rest:""
  in
  [ registry_row registry; { Row.view = View.none; address = None } ] @ tree
;;

let count_nodes snapshot =
  let rec count (node : Snapshot.Node.t) =
    1 + List.sum (module Int) node.children ~f:count
  in
  count snapshot.Snapshot.root_node
;;

let scroll_limit rows ~height = Int.max 0 (List.length rows - height)

let view ~width ~height ~snapshot ~registry ~new_addresses ~scroll =
  let all_rows = rows ~snapshot ~registry ~new_addresses in
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

let address_at ~snapshot ~registry ~new_addresses ~scroll ~height ~row =
  let all_rows = rows ~snapshot ~registry ~new_addresses in
  let scroll = Int.min scroll (scroll_limit all_rows ~height:(height - 2)) in
  match List.nth all_rows (row + scroll) with
  | Some { Row.address; view = _ } -> address
  | None -> None
;;
