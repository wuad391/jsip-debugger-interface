open! Core
open Jsip_types
open Jsip_replay
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View
module Layer = Snapshot.Ds_type.Layer

(* which card is chosen and which one the keyboard is aiming at. Both are
   addresses because that is what a click yields and what the canvas keys
   cards by; either may be absent, and while you aim they are both on screen
   — blue behind, orange ahead. *)
module Selection = struct
  type t =
    { selected : Snapshot.Address.t option
    ; cursor : Snapshot.Address.t option
    }
  [@@deriving sexp_of, equal]

  let none = { selected = None; cursor = None }

  let is address slot =
    match slot with
    | Some other -> Snapshot.Address.equal other address
    | None -> false
  ;;

  let selects t address = is address t.selected
  let aims_at t address = is address t.cursor
end

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, equal]
end

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

(* everything reference-following needs while a step's canvas is drawn: the
   registry, and which structures are already on it. Only [Id] blocks
   reference tracked structures — an [Address] block is a pointer the walker
   chose not to decode. *)
module Context = struct
  type t =
    { by_id : Replay.Structure.t Int.Map.t
    ; by_address : Replay.Structure.t Snapshot.Address.Map.t
    (** for tagging a card that is some structure's root, not for resolving
        [Address] blocks *)
    ; nodes : Replay.Nodes.t
    ; drawn : Int.Hash_set.t (** structures already placed on the canvas *)
    ; drawn_nodes : Int.Hash_set.t
    (** node ids already drawn: the wire shares blocks between structures and
        versions (and a payload may even cycle), so a second occurrence
        points at the first instead of redrawing it *)
    ; new_addresses : Snapshot.Address.Set.t
    ; selection : Selection.t
    (** which card is blue and which is orange — geometry depends on it,
        because only the selected card spells out its address *)
    }

  let create ~structures ~nodes ~new_addresses ~selection =
    { by_id =
        Int.Map.of_alist_reduce
          (List.map structures ~f:(fun (structure : Replay.Structure.t) ->
             structure.id, structure))
          ~f:(fun first (_ : Replay.Structure.t) -> first)
    ; by_address =
        Snapshot.Address.Map.of_alist_reduce
          (List.map structures ~f:(fun (structure : Replay.Structure.t) ->
             structure.address, structure))
          ~f:(fun first (_ : Replay.Structure.t) -> first)
    ; nodes
    ; drawn = Int.Hash_set.create ()
    ; drawn_nodes = Int.Hash_set.create ()
    ; new_addresses
    ; selection
    }
  ;;

  (* an [Id] names a node the dump defined earlier — sometimes a tracked
     structure's root, sometimes a shared payload block *)
  let node t (block : Snapshot.Block.t) =
    match block with
    | Id id -> Replay.Nodes.find t.nodes id
    | Address _ | Int _ | Float _ | String _ | Int32 _ | Int64 _
    | Nativeint _ | Float_array _ ->
      None
  ;;

  let structure t (block : Snapshot.Block.t) =
    match block with
    | Id id -> Map.find t.by_id id
    | Address _ | Int _ | Float _ | String _ | Int32 _ | Int64 _
    | Nativeint _ | Float_array _ ->
      None
  ;;
end

(* How to read one node: either we are on the structure's own skeleton — at a
   known layer, interior edges leading one layer deeper (the last layer
   repeats) — or below a payload edge, where blocks are generic user data
   with numeric positional labels and no masks. *)
module Mode = struct
  type t =
    | Ds of Layer.t list
    | Payload

  let deeper layers =
    match layers with [] | [ _ ] -> layers | (_ : Layer.t) :: rest -> rest
  ;;
end

(* queue cells keep the wire's numeric labels; show them as the fields they
   are *)
let pretty_label ~ds_type ~(layer : Layer.t) label =
  match (ds_type : Snapshot.Ds_type.t), layer, label with
  | Queue, Fixed { labels = [ "0"; "1" ]; _ }, "0" -> "v"
  | Queue, Fixed { labels = [ "0"; "1" ]; _ }, "1" -> "next"
  | (Map | Set | Queue | Hashtbl), (Fixed _ | Array_elements), label -> label
;;

(* one node's outgoing edges and inline fields, by the layer contract *)
module Edge = struct
  type t =
    | Nil (** an empty interior slot *)
    | Child of Snapshot.Node.t * Mode.t
    | Ref of Replay.Structure.t
    (** a tracked structure reached through a reference *)
    | Shared of int
    (** a node already drawn elsewhere on this canvas: the wire shares it, so
        the canvas points at it rather than drawing it twice *)
end

let node_edges
  (node : Snapshot.Node.t)
  ~(mode : Mode.t)
  ~(context : Context.t)
  =
  let block_value label =
    List.Assoc.find node.block label ~equal:String.equal
  in
  let children = Queue.of_list node.children in
  let take_child mode =
    match Queue.dequeue children with
    | Some child -> Some (Edge.Child (child, mode))
    | None -> None
  in
  (* An [Id] leaf names a node the dump defined earlier. If it is a tracked
     structure's root, its whole tree links in here; otherwise it is a shared
     block, which draws here the first time and points back every time after.
     Either way the slot stops printing as a value. *)
  let claim_reference block =
    match Context.structure context block with
    | Some (structure : Replay.Structure.t) ->
      (match Hash_set.mem context.drawn structure.id with
       | false ->
         Hash_set.add context.drawn structure.id;
         Some (Edge.Ref structure)
       | true -> Some (Edge.Shared structure.id))
    | None ->
      (match Context.node context block with
       | None -> None
       | Some (definition : Snapshot.Node.t) ->
         (match Hash_set.mem context.drawn_nodes definition.id with
          | false -> Some (Edge.Child (definition, Mode.Payload))
          | true -> Some (Edge.Shared definition.id)))
  in
  (* labeled edges in field order; anything the summary should not print (a
     slot drawn as an edge) lands in [hidden] *)
  let edges, hidden =
    match mode with
    | Ds (Fixed { labels; interior; payload } :: _ as layers) ->
      let is_interior label = List.mem interior label ~equal:String.equal in
      let is_payload label = List.mem payload label ~equal:String.equal in
      List.fold labels ~init:([], []) ~f:(fun (edges, hidden) label ->
        match is_interior label, is_payload label with
        | true, _ ->
          (match block_value label with
           | None ->
             (match take_child (Mode.Ds (Mode.deeper layers)) with
              | Some edge -> (label, edge) :: edges, label :: hidden
              | None -> edges, hidden)
           | Some (Snapshot.Block.Int 0) ->
             (label, Edge.Nil) :: edges, label :: hidden
           | Some (Snapshot.Block.Id id as block) ->
             (* a shared interior slot: the subtree lives under that id *)
             let edge =
               match Context.node context block with
               | Some definition
                 when not (Hash_set.mem context.drawn_nodes definition.id) ->
                 Edge.Child (definition, Mode.Ds (Mode.deeper layers))
               | Some _ | None -> Edge.Shared id
             in
             (label, edge) :: edges, label :: hidden
           | Some (_ : Snapshot.Block.t) -> edges, hidden)
        | false, true ->
          (match block_value label with
           | None ->
             (match take_child Mode.Payload with
              | Some edge -> (label, edge) :: edges, label :: hidden
              | None -> edges, hidden)
           | Some block ->
             (match claim_reference block with
              | Some edge -> (label, edge) :: edges, label :: hidden
              | None -> edges, hidden))
        | false, false -> edges, hidden)
    | Ds (Array_elements :: _ as layers) | Ds ([] as layers) ->
      (* numeric labels; total field count = kept + walked. Empty slots
         ([Int 0]) stay silent — a hashtbl's bucket array is mostly empties *)
      let total = List.length node.block + List.length node.children in
      let next_mode =
        match layers with
        | [] -> Mode.Payload
        | (_ : Layer.t) :: _ -> Mode.Ds (Mode.deeper layers)
      in
      List.init total ~f:Int.to_string
      |> List.fold ~init:([], []) ~f:(fun (edges, hidden) label ->
        match block_value label with
        | None ->
          (match take_child next_mode with
           | Some edge -> (label, edge) :: edges, label :: hidden
           | None -> edges, hidden)
        | Some (Snapshot.Block.Int 0) -> edges, label :: hidden
        | Some (_ : Snapshot.Block.t) -> edges, hidden)
    | Payload ->
      let total = List.length node.block + List.length node.children in
      List.init total ~f:Int.to_string
      |> List.fold ~init:([], []) ~f:(fun (edges, hidden) label ->
        match block_value label with
        | None ->
          (match take_child Mode.Payload with
           | Some edge -> (label, edge) :: edges, label :: hidden
           | None -> edges, hidden)
        | Some block ->
          (match claim_reference block with
           | Some edge -> (label, edge) :: edges, label :: hidden
           | None -> edges, hidden))
  in
  let unclaimed =
    List.map (Queue.to_list children) ~f:(fun child ->
      "", Edge.Child (child, Mode.Payload))
  in
  List.rev edges @ unclaimed, hidden
;;

(* what the card says, per layer: [key → data] where the layer holds a pair
   of payload fields, [length n]/[size n] for counters, the bare value for
   single-payload layers, joined positions for user data *)
let summary_spans (node : Snapshot.Node.t) ~(mode : Mode.t) ~hidden_labels =
  let hidden label = List.mem hidden_labels label ~equal:String.equal in
  let visible =
    List.filter node.block ~f:(fun (label, (_ : Snapshot.Block.t)) ->
      not (hidden label))
  in
  let display (label, block) = label, Snapshot.Block.display block in
  match mode with
  | Ds (Fixed { payload; labels = _; interior = _ } :: _) ->
    let payload_fields =
      List.filter visible ~f:(fun (label, (_ : Snapshot.Block.t)) ->
        List.mem payload label ~equal:String.equal)
      |> List.map ~f:display
    in
    let leftovers =
      List.filter visible ~f:(fun (label, (_ : Snapshot.Block.t)) ->
        not (List.mem payload label ~equal:String.equal))
      |> List.map ~f:display
      |> List.concat_map ~f:(fun (label, value) ->
        [ `Label, [%string "  %{label}="]; `Value, value ])
    in
    let main =
      match payload_fields with
      | [ ((_ : string), key); ((_ : string), data) ] ->
        [ `Key, key; `Arrow, " → "; `Value, data ]
      | [ ((("length" | "size") as label), value) ] ->
        [ `Label, [%string "%{label} "]; `Value, value ]
      | [ ((_ : string), value) ] -> [ `Value, value ]
      | [] -> [ `Arrow, "·" ]
      | fields ->
        List.concat_map fields ~f:(fun (label, value) ->
          [ `Label, [%string "%{label}="]; `Value, value ])
    in
    main @ leftovers
  | Ds (Array_elements :: _) | Ds [] ->
    let total = List.length node.block + List.length node.children in
    [ `Label, "slots "; `Value, Int.to_string total ]
  | Payload ->
    (match
       List.map visible ~f:(fun ((_ : string), block) ->
         Snapshot.Block.display block)
     with
     | [] -> [ `Arrow, "·" ]
     | values -> [ `Value, String.concat values ~sep:", " ])
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

(* What a click can fold: a whole structure behind its section header (the
   name · kind summary), or any node's children behind its card. Node paths
   are edge positions from the owning structure's root, so a fold survives
   re-walks and re-parenting of the drawn tree. *)
module Fold = struct
  module T = struct
    type t =
      | Structure of int
      | Node of int * int list
    [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

module Toggle = struct
  type t =
    { x : int
    ; y : int
    ; fold : Fold.t
    }

  let shift t ~dx ~dy = { t with x = t.x + dx; y = t.y + dy }
end

let glyph_of ~folded = match folded with true -> "▸" | false -> "▾"

(* the node card — the structure's name riding the border's top left, a green
   [new] tag riding the top right for this step's allocations, and (when
   folded) how many nodes are tucked away, spelled out below the card. The
   outline is blue on the selected card, orange on the one the keyboard is
   aiming at, and the calmer card blue everywhere else; the same three states
   pick the wash.

   Only the selected card spells out its address. Every card carrying twelve
   hex digits set the whole diagram's width from a string nobody was reading;
   showing it on the one card whose identity is actually in question buys
   three columns and a row back on all the others.
   {v
   ┌ m ──────────── new ┐
   │ "a" → 2            │
   │ 0x763be65ee878     │   ← selected only
   └────────────────────┘
        ⋯ 3 hidden
   v} *)
let node_box
  (node : Snapshot.Node.t)
  ~mode
  ~hidden_labels
  ~(context : Context.t)
  ~fold_glyph
  ~hidden_count
  =
  let is_new = Set.mem context.new_addresses node.virtual_address in
  let root_structure = Map.find context.by_address node.virtual_address in
  let selection = context.selection in
  let is_selected = Selection.selects selection node.virtual_address in
  let is_cursor = Selection.aims_at selection node.virtual_address in
  let border =
    Theme.fg'
      (match is_cursor, is_selected with
       | true, _ -> Theme.cursor
       | false, true -> Theme.highlight
       | false, false -> Theme.card_border)
  in
  let summary =
    View.hcat
      (List.map (summary_spans node ~mode ~hidden_labels) ~f:span_view)
  in
  (* both picked-out cards spell their address out: the blue one because it
     is the chosen one, the orange one because knowing what you are about to
     choose is the point of aiming *)
  let address =
    match is_selected || is_cursor with
    | false -> None
    | true ->
      Some
        (View.text
           ~attrs:(Theme.fg' Theme.secondary)
           (Snapshot.Address.display node.virtual_address))
  in
  (* border riders: the structure's name left, a fresh allocation right *)
  let name_tag =
    match root_structure with
    | None -> View.none
    | Some structure ->
      (* the name reads as the card's label, not as chrome — white, and bold
         where the card is chosen or aimed at *)
      let attrs =
        match is_selected || is_cursor with
        | true -> [ Theme.fg Theme.text; Attr.bold ]
        | false -> Theme.fg' Theme.text
      in
      View.text ~attrs [%string " %{Replay.Structure.display structure} "]
  in
  let new_tag =
    match is_new with
    | true -> View.text ~attrs:(Theme.fg' Theme.fresh) " new "
    | false -> View.none
  in
  let riders_width = View.width name_tag + View.width new_tag in
  let inner =
    List.reduce_exn
      ~f:Int.max
      (View.width summary
       :: (riders_width - 2)
       :: List.map (Option.to_list address) ~f:View.width)
  in
  (* every row is exactly [inner + 4] cells, so the wash covers the card and
     nothing else *)
  let card_width = inner + 4 in
  let top =
    View.hcat
      [ View.text ~attrs:border "┌"
      ; name_tag
      ; View.text
          ~attrs:border
          (Panel.repeat "─" ~width:(inner + 2 - riders_width))
      ; new_tag
      ; View.text ~attrs:border "┐"
      ]
  in
  let bottom =
    View.text
      ~attrs:border
      [%string "└%{Panel.repeat \"─\" ~width:(inner + 2)}┘"]
  in
  let content line =
    View.hcat
      [ View.text ~attrs:border "│ "
      ; Panel.fit line ~width:inner ~height:1
      ; View.text ~attrs:border " │"
      ]
  in
  let rows =
    [ top; content summary ]
    @ List.map (Option.to_list address) ~f:content
    @ [ bottom ]
  in
  let card =
    Panel.fit (View.vcat rows) ~width:card_width ~height:(List.length rows)
  in
  let card =
    match is_cursor, is_selected with
    | true, _ ->
      View.with_colors' ~fill_backdrop:true ~bg:Theme.cursor_bg card
    | false, true ->
      View.with_colors' ~fill_backdrop:true ~bg:Theme.highlight_bg card
    | false, false -> card
  in
  (* what a fold hides is said below the card, not squeezed into its border *)
  let card =
    match hidden_count with
    | 0 -> card
    | n ->
      let note =
        View.text ~attrs:(Theme.fg' Theme.text) [%string "⋯ %{n#Int} hidden"]
      in
      let indent = Int.max 0 ((card_width - View.width note) / 2) in
      View.vcat [ card; View.pad ~l:indent note ]
  in
  (* the fold glyph sits in a reserved column left of every card, so sibling
     math stays uniform whether or not a card can fold *)
  let glyph =
    match fold_glyph with
    | None -> View.text " "
    | Some folded ->
      View.text ~attrs:(Theme.fg' Theme.secondary) (glyph_of ~folded)
  in
  View.zcat [ View.pad ~l:1 card; glyph ]
;;

(* An empty slot is still a slot, so it gets a card too — dotted and grayed,
   the same three rows as a real one. A bare [∅] hanging off a rail read as
   an annotation on the edge; a box reads as what it is, the thing the
   pointer does not point at. *)
let nil_box =
  let attrs = Theme.fg' Theme.ghost in
  View.vcat
    [ View.text ~attrs "┌┄┄┄┐"
    ; View.text ~attrs "┆ ∅ ┆"
    ; View.text ~attrs "└┄┄┄┘"
    ]
;;

let sibling_gap = 3

(* the ┌──┴──┐ rail between a parent and its children, hooks at each child's
   center. Light stroke, but a brighter gray than the surrounding chrome: the
   rails are the diagram's edges — the actual pointers — so they should read
   ahead of the pane's dividers without turning into bars themselves. *)
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
    ~attrs:(Theme.fg' Theme.rail)
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
   connecting the card to each child's center. A payload field holding an
   [Id] into the registry links that structure's whole tree in as a child —
   each structure is drawn once, so a second reference (or a cycle) stays an
   inline [#id]. A folded card keeps itself and hides everything below — on
   the expanded layout's footprint, so folding reflows nothing else: the
   freed space is vertical only. Returns the canvas, the card's center
   column, every card's position, and every fold glyph's position. *)
let rec tree
  (node : Snapshot.Node.t)
  ~ds_type
  ~(mode : Mode.t)
  ~(context : Context.t)
  ~folds
  ~structure_id
  ~path
  : View.t * int * Placed.t list * Toggle.t list
  =
  let edges, hidden_labels = node_edges node ~mode ~context in
  let edges =
    (* a leaf keeps its empty slots to itself *)
    match
      List.for_all edges ~f:(fun ((_ : string), edge) ->
        match edge with
        | Edge.Nil -> true
        | Edge.Child _ | Edge.Ref _ | Edge.Shared _ -> false)
    with
    | true -> []
    | false -> edges
  in
  let fold = Fold.Node (structure_id, List.rev path) in
  let collapsible = not (List.is_empty edges) in
  let folded = collapsible && Set.mem folds fold in
  let fold_glyph =
    match collapsible with true -> Some folded | false -> None
  in
  let leaf_box ~hidden_count =
    node_box node ~mode ~hidden_labels ~context ~fold_glyph ~hidden_count
  in
  Hash_set.add context.drawn_nodes node.id;
  match edges with
  | [] ->
    let box = leaf_box ~hidden_count:0 in
    let box_width = View.width box in
    ( box
    , box_width / 2
    , [ { Placed.x = 0
        ; y = 0
        ; width = box_width
        ; height = View.height box
        ; address = node.virtual_address
        }
      ]
    , [] )
  | edges ->
    let current_layer =
      match mode with
      | Ds (layer :: _) -> Some layer
      | Ds [] | Payload -> None
    in
    let rendered =
      List.mapi edges ~f:(fun index (label, edge) ->
        let label =
          match current_layer with
          | Some layer -> pretty_label ~ds_type ~layer label
          | None -> label
        in
        match edge with
        | Edge.Nil -> label, (nil_box, View.width nil_box / 2, [], [])
        | Edge.Shared id ->
          (* the node is on the canvas already — say which one *)
          let stub =
            View.text ~attrs:(Theme.fg' Theme.muted) [%string "↗ #%{id#Int}"]
          in
          label, (stub, View.width stub / 2, [], [])
        | Edge.Child (child, mode) ->
          ( label
          , tree
              child
              ~ds_type
              ~mode
              ~context
              ~folds
              ~structure_id
              ~path:(index :: path) )
        | Edge.Ref (structure : Replay.Structure.t) ->
          ( label
          , tree
              structure.snapshot.root_node
              ~ds_type:structure.snapshot.ds_type
              ~mode:
                (Mode.Ds (Snapshot.Ds_type.layers structure.snapshot.ds_type))
              ~context
              ~folds
              ~structure_id:structure.id
              ~path:[] ))
    in
    (* children lay out even when folded: their claims must hold (a folded
       queue cell keeps its map hidden), their card count is the [⋯ n hidden]
       tag, and their footprint is what keeps the rest of the diagram still
       when this card folds *)
    let (_ : int), placed_children =
      List.fold_map
        rendered
        ~init:0
        ~f:(fun x (label, (view, center, placed, toggles)) ->
          ( x + View.width view + sibling_gap
          , (label, view, x, x + center, placed, toggles) ))
    in
    let hidden_count =
      match folded with
      | false -> 0
      | true ->
        List.sum
          (module Int)
          placed_children
          ~f:
            (fun
              ( (_ : string)
              , (_ : View.t)
              , (_ : int)
              , (_ : int)
              , placed
              , (_ : Toggle.t list) )
            -> List.length placed)
    in
    Hash_set.add context.drawn_nodes node.id;
    let box = leaf_box ~hidden_count in
    (* geometry always follows the unfolded card, so folding (whose ⋯ tag can
       widen the border) never moves the card's center *)
    let box_width = View.width (leaf_box ~hidden_count:0) in
    let box_height = View.height box in
    let centers =
      List.map
        placed_children
        ~f:
          (fun
            ( (_ : string)
            , (_ : View.t)
            , (_ : int)
            , center
            , (_ : Placed.t list)
            , (_ : Toggle.t list) )
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
    let box_placed =
      { Placed.x = 0
      ; y = 0
      ; width = View.width box
      ; height = box_height
      ; address = node.virtual_address
      }
    in
    let box_toggles = [ { Toggle.x = 0; y = 0; fold } ] in
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
               , (_ : Placed.t list)
               , (_ : Toggle.t list) )
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
    let children_views, children_placed, children_toggles =
      List.fold
        placed_children
        ~init:([], [], [])
        ~f:
          (fun
            (views, all_placed, all_toggles)
            ((_ : string), view, x, (_ : int), placed, toggles)
          ->
          let x = x + child_shift in
          ( View.pad ~l:x ~t:children_y view :: views
          , List.map placed ~f:(Placed.shift ~dx:x ~dy:children_y)
            @ all_placed
          , List.map toggles ~f:(Toggle.shift ~dx:x ~dy:children_y)
            @ all_toggles ))
    in
    let expanded =
      View.zcat
        ((View.pad ~l:parent_x box
          :: List.mapi rail_rows ~f:(fun i row ->
            View.pad ~t:(box_height + i) row))
         @ children_views)
    in
    (match folded with
     | false ->
       ( expanded
       , parent_center
       , Placed.shift box_placed ~dx:parent_x ~dy:0 :: children_placed
       , List.map box_toggles ~f:(Toggle.shift ~dx:parent_x ~dy:0)
         @ children_toggles )
     | true ->
       (* the card alone, at the very column it occupies expanded (the [⋯]
          note lives below the border, so folding cannot change the card's
          width) — nothing else in the diagram moves *)
       let folded_x = parent_x in
       let canvas =
         View.zcat
           [ View.pad ~l:folded_x box
           ; View.transparent_rectangle
               ~width:(View.width expanded)
               ~height:1
           ]
       in
       ( canvas
       , parent_center
       , [ Placed.shift box_placed ~dx:folded_x ~dy:0 ]
       , List.map box_toggles ~f:(Toggle.shift ~dx:folded_x ~dy:0) ))
;;

(* the section header over one structure's tree: a fold glyph, then its name
   (or [#id]) and kind — which is exactly the summary a folded structure
   collapses to. The one this step's event walked reads in the highlight
   blue. *)
let structure_header (structure : Replay.Structure.t) ~folded =
  let label =
    [%string
      "%{Replay.Structure.display structure} · %{Snapshot.Ds_type.display \
       structure.snapshot.ds_type}"]
  in
  let label =
    match structure.ty with
    | None -> label
    | Some ty -> [%string "%{label} %{Type_info.display ty}"]
  in
  let label_attrs =
    match structure.is_current with
    | true -> [ Theme.fg Theme.highlight_deep; Attr.bold ]
    | false -> Theme.fg' Theme.muted
  in
  View.hcat
    [ View.text ~attrs:(Theme.fg' Theme.secondary) (glyph_of ~folded)
    ; View.text " "
    ; View.text ~attrs:label_attrs label
    ]
;;

(* Every live structure, stacked: a header, its tree (unless the header's
   fold hides it), a breathing row. A structure referenced from another one
   is drawn inside its referrer's tree instead of as its own section — the
   registry still decides what is alive, only the placement moves. *)
let layout ~structures ~nodes ~new_addresses ~folds ~selection =
  let context =
    Context.create ~structures ~nodes ~new_addresses ~selection
  in
  let referenced =
    let rec walk (owner : Replay.Structure.t) (node : Snapshot.Node.t) acc =
      let acc =
        List.fold node.block ~init:acc ~f:(fun acc ((_ : string), block) ->
          match Context.structure context block with
          | Some (target : Replay.Structure.t) when target.id <> owner.id ->
            Set.add acc target.id
          | Some _ | None -> acc)
      in
      List.fold node.children ~init:acc ~f:(fun acc child ->
        walk owner child acc)
    in
    List.fold structures ~init:Int.Set.empty ~f:(fun acc structure ->
      walk structure structure.Replay.Structure.snapshot.root_node acc)
  in
  let section
    (views, all_placed, all_toggles, y)
    (structure : Replay.Structure.t)
    =
    match Hash_set.mem context.drawn structure.id with
    | true -> views, all_placed, all_toggles, y
    | false ->
      Hash_set.add context.drawn structure.id;
      let folded = Set.mem folds (Fold.Structure structure.id) in
      let header = structure_header structure ~folded in
      let header_toggle =
        { Toggle.x = 0; y; fold = Fold.Structure structure.id }
      in
      (* the tree lays out either way so its reference claims hold — a folded
         structure keeps what it references hidden with it *)
      let canvas, (_ : int), placed, toggles =
        tree
          (Replay.Structure.current_root structure)
          ~ds_type:structure.snapshot.ds_type
          ~mode:
            (Mode.Ds (Snapshot.Ds_type.layers structure.snapshot.ds_type))
          ~context
          ~folds
          ~structure_id:structure.id
          ~path:[]
      in
      (match folded with
       | true ->
         (* the header is the whole summary *)
         ( View.pad ~t:y header :: views
         , all_placed
         , header_toggle :: all_toggles
         , y + 2 )
       | false ->
         let views =
           View.pad ~t:(y + 1) canvas :: View.pad ~t:y header :: views
         in
         let placed =
           List.map placed ~f:(Placed.shift ~dx:0 ~dy:(y + 1)) @ all_placed
         in
         let toggles =
           header_toggle
           :: (List.map toggles ~f:(Toggle.shift ~dx:0 ~dy:(y + 1))
               @ all_toggles)
         in
         views, placed, toggles, y + 1 + View.height canvas + 1)
  in
  let top_level =
    List.filter structures ~f:(fun (structure : Replay.Structure.t) ->
      not (Set.mem referenced structure.id))
  in
  let acc = List.fold top_level ~init:([], [], [], 0) ~f:section in
  (* mutually-referencing structures have no unreferenced root; anything
     still undrawn gets its own section after all *)
  let views, placed, toggles, (_ : int) =
    List.fold structures ~init:acc ~f:section
  in
  View.zcat views, placed, toggles
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

(* The wheel and PgUp/PgDn set the scroll, but the cursor overrides it: a
   card you cannot see is a card you cannot aim at. The adjustment is the
   smallest one that brings it into the body, so scrolling by hand and then
   moving the cursor a step does not throw the view somewhere else. *)
let follow_cursor placed ~body_height ~scroll ~(selection : Selection.t) =
  match selection.cursor with
  | None -> scroll
  | Some address ->
    (match
       List.find placed ~f:(fun (card : Placed.t) ->
         Snapshot.Address.equal card.address address)
     with
     | None -> scroll
     | Some card ->
       Int.max (card.y + card.height - body_height) (Int.min scroll card.y))
;;

let clamp_scroll canvas ~height ~scroll =
  Int.max
    0
    (Int.min
       scroll
       (Int.max 0 (View.height canvas - (height - Panel.header_height))))
;;

(* every entry point scrolls the same way, so hit-testing lands where the eye
   does *)
let resolve_scroll canvas placed ~height ~scroll ~selection =
  follow_cursor
    placed
    ~body_height:(height - Panel.header_height)
    ~scroll
    ~selection
  |> fun scroll -> clamp_scroll canvas ~height ~scroll
;;

let view
  ~width
  ~height
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~scroll
  ~selection
  =
  let canvas, placed, (_ : Toggle.t list) =
    layout ~structures ~nodes ~new_addresses ~folds ~selection
  in
  let scroll = resolve_scroll canvas placed ~height ~scroll ~selection in
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

let toggle_at
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~scroll
  ~selection
  ~height
  ~x
  ~y
  =
  let canvas, placed, toggles =
    layout ~structures ~nodes ~new_addresses ~folds ~selection
  in
  let scroll = resolve_scroll canvas placed ~height ~scroll ~selection in
  let y = y + scroll in
  List.find toggles ~f:(fun (toggle : Toggle.t) ->
    x = toggle.x && y = toggle.y)
  |> Option.map ~f:(fun (toggle : Toggle.t) -> toggle.fold)
;;

let address_at
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~scroll
  ~selection
  ~height
  ~x
  ~y
  =
  let canvas, placed, (_ : Toggle.t list) =
    layout ~structures ~nodes ~new_addresses ~folds ~selection
  in
  let scroll = resolve_scroll canvas placed ~height ~scroll ~selection in
  List.find placed ~f:(Placed.contains ~x ~y:(y + scroll))
  |> Option.map ~f:(fun (placed : Placed.t) -> placed.address)
;;

let cards ~structures ~nodes ~new_addresses ~folds ~selection =
  let (_ : View.t), placed, (_ : Toggle.t list) =
    layout ~structures ~nodes ~new_addresses ~folds ~selection
  in
  placed
;;

(* Where a card sits for the purpose of aiming at it: its center. Cards are
   wide and short, so a plain euclidean nearest-neighbour drifts sideways
   when you press [w] — the along-axis gap is weighted so the direction you
   asked for dominates, and the cross-axis gap only breaks ties between
   candidates the same distance away. *)
let move_cursor
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~selection
  ~(direction : Direction.t)
  =
  let placed = cards ~structures ~nodes ~new_addresses ~folds ~selection in
  let center (card : Placed.t) =
    card.x + (card.width / 2), card.y + (card.height / 2)
  in
  let from =
    Option.first_some selection.cursor selection.selected
    |> Option.bind ~f:(fun address ->
      List.find placed ~f:(fun (card : Placed.t) ->
        Snapshot.Address.equal card.address address))
  in
  match from with
  | None ->
    (* nothing aimed at yet: start at the topmost card *)
    List.min_elt placed ~compare:(fun a b ->
      [%compare: int * int] (a.y, a.x) (b.y, b.x))
    |> Option.map ~f:(fun (card : Placed.t) -> card.address)
  | Some origin ->
    let ox, oy = center origin in
    let scored =
      List.filter_map placed ~f:(fun (card : Placed.t) ->
        let x, y = center card in
        let along, across =
          match direction with
          | Up -> oy - y, abs (x - ox)
          | Down -> y - oy, abs (x - ox)
          | Left -> ox - x, abs (y - oy)
          | Right -> x - ox, abs (y - oy)
        in
        match along > 0 with
        | false -> None
        | true -> Some (card.address, (along + (across * 2), along, across)))
    in
    List.min_elt scored ~compare:(fun (_, a) (_, b) ->
      [%compare: int * int * int] a b)
    |> Option.map ~f:fst
;;

(* the canvas row a card starts on, so the app can scroll it into view *)
let row_of ~structures ~nodes ~new_addresses ~folds ~selection address =
  cards ~structures ~nodes ~new_addresses ~folds ~selection
  |> List.find ~f:(fun (card : Placed.t) ->
    Snapshot.Address.equal card.address address)
  |> Option.map ~f:(fun (card : Placed.t) -> card.y, card.height)
;;
