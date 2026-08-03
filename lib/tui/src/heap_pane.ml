open! Core
open Jsip_types
open Jsip_replay
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View
module Shape = Snapshot.Ds_type.Shape

(* Which drawing of a node the keyboard is standing on. A node can be on the
   canvas twice — its own card, and a [↗] pointer at it from a structure that
   shares it — and those are two places even though they are one object, so a
   position has to say which one it means. Keyed like a fold: the owning
   structure, and the edge path down to the card. *)
module Site = struct
  type t =
    { structure : int
    ; path : int list
    }
  [@@deriving sexp_of, equal]
end

(* a node, and which drawing of it *)
module Spot = struct
  type t =
    { address : Snapshot.Address.t
    ; site : Site.t
    }
  [@@deriving sexp_of, equal]
end

(* which card is chosen and which one the keyboard is aiming at; either may
   be absent, and while you aim they are both on screen — blue behind, orange
   ahead. *)
module Selection = struct
  type t =
    { selected : Spot.t option
    ; cursor : Spot.t option
    }
  [@@deriving sexp_of, equal]

  let none = { selected = None; cursor = None }

  (* How a card is picked out. The one the keyboard is standing on wears the
     full treatment — wash, address, bright border. Any other drawing of the
     same node wears the border color alone: enough to find it across the
     pane, without a second card shouting from wherever it happens to be. *)
  module Mark = struct
    type t =
      | Plain
      | Linked_to_selected
      | Linked_to_cursor
      | Selected
      | Cursor
  end

  let where spot ~address ~site =
    match spot with
    | None -> `Elsewhere
    | Some { Spot.address = other; site = other_site } ->
      (match Site.equal other_site site with
       | true -> `Here
       | false ->
         (match Snapshot.Address.equal other address with
          | true -> `Linked
          | false -> `Elsewhere))
  ;;

  let mark t ~address ~site =
    match where t.cursor ~address ~site, where t.selected ~address ~site with
    | `Here, _ -> Mark.Cursor
    | (`Linked | `Elsewhere), `Here -> Mark.Selected
    | `Linked, (`Linked | `Elsewhere) -> Mark.Linked_to_cursor
    | `Elsewhere, `Linked -> Mark.Linked_to_selected
    | `Elsewhere, `Elsewhere -> Mark.Plain
  ;;
end

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, equal]
end

(* where a node's card landed on the tree canvas — for click-to-jump, and for
   the cursor, which walks the tree rather than the picture: [depth], [site]
   and [parent] are the diagram's own structure, so aiming does not shift
   when the drawing does *)
module Placed = struct
  type t =
    { x : int
    ; y : int
    ; width : int
    ; height : int
    ; address : Snapshot.Address.t
    ; site : Site.t (** this card, as opposed to any other drawing of it *)
    ; depth : int (** card rows between this one and its tree's root *)
    ; parent : Site.t option (** [None] on a section's root *)
    ; is_pointer : bool
    (** a [↗] at the node rather than the node's own card. Both are places
        the cursor can stand; only one of them is where the node lives. *)
    }

  let spot t = { Spot.address = t.address; site = t.site }

  let contains t ~x ~y =
    x >= t.x && x < t.x + t.width && y >= t.y && y < t.y + t.height
  ;;

  let shift t ~dx ~dy = { t with x = t.x + dx; y = t.y + dy }
end

let card_at placed site =
  List.find placed ~f:(fun (card : Placed.t) -> Site.equal card.site site)
;;

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
    | Nativeint _ | Float_array _ | Child ->
      None
  ;;

  let structure t (block : Snapshot.Block.t) =
    match block with
    | Id id -> Map.find t.by_id id
    | Address _ | Int _ | Float _ | String _ | Int32 _ | Int64 _
    | Nativeint _ | Float_array _ | Child ->
      None
  ;;
end

(* How to read one node: either we are on the structure's own skeleton, where
   {!Snapshot.Ds_type.shape} says what its labels mean, or below a payload
   edge, where blocks are generic user data with numeric positional labels
   and no roles. Once in payload the walk stays there — except through a
   reference to another tracked structure, which starts its own skeleton
   again. *)
module Mode = struct
  type t =
    | Ds of Snapshot.Ds_type.t
    | Payload

  let shape t ~(node : Snapshot.Node.t) =
    match t with
    | Payload -> None
    | Ds ds_type ->
      Some
        (Snapshot.Ds_type.shape ds_type ~labels:(List.map node.block ~f:fst))
  ;;

  (* interior slots stay on the skeleton, payload slots leave it for good *)
  let through t ~role =
    match role, t with
    | `Interior, Ds ds_type -> Ds ds_type
    | `Interior, Payload | `Payload, (Ds _ | Payload) -> Payload
  ;;
end

(* cons cells keep the wire's numeric labels; show them as the fields they
   are *)
let pretty_label ~(shape : Shape.t option) label =
  match shape, label with
  | Some (Fields { interior = [ "1" ]; payload = [ "0" ] }), "0" -> "v"
  | Some (Fields { interior = [ "1" ]; payload = [ "0" ] }), "1" -> "next"
  | (Some (Fields _ | Elements _) | None), label -> label
;;

(* one node's outgoing edges and inline fields, by its shape *)
module Edge = struct
  (* a node already drawn elsewhere on this canvas: the wire shares it, so
     the canvas points at it rather than drawing it twice. The pointer
     carries enough to name what it points at the way that card does — the
     node the id defines, and the mode this slot would have read it in. *)
  type shared =
    { id : int
    ; node : Snapshot.Node.t option
    ; mode : Mode.t
    }

  type t =
    | Nil (** an empty interior slot *)
    | Child of Snapshot.Node.t * Mode.t
    | Ref of Replay.Structure.t
    (** a tracked structure reached through a reference *)
    | Shared of shared
end

let node_edges
  (node : Snapshot.Node.t)
  ~(mode : Mode.t)
  ~(context : Context.t)
  =
  let shape = Mode.shape mode ~node in
  (* every label the wire kept is one or the other, so what is not interior
     is the user's data *)
  let role label =
    match shape with
    | None -> `Payload
    | Some (Shape.Elements { interior }) ->
      (match interior with true -> `Interior | false -> `Payload)
    | Some (Shape.Fields { interior; payload = _ }) ->
      (match List.mem interior label ~equal:String.equal with
       | true -> `Interior
       | false -> `Payload)
  in
  let mode_at label = Mode.through mode ~role:(role label) in
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
  let claim_reference block ~mode =
    match Context.structure context block with
    | Some (structure : Replay.Structure.t) ->
      (match Hash_set.mem context.drawn structure.id with
       | false ->
         Hash_set.add context.drawn structure.id;
         Some (Edge.Ref structure)
       | true ->
         Some
           (Edge.Shared
              { id = structure.id
              ; node = Some structure.snapshot.root_node
              ; mode = Mode.Ds structure.snapshot.ds_type
              }))
    | None ->
      (match Context.node context block with
       | None -> None
       | Some (definition : Snapshot.Node.t) ->
         (match Hash_set.mem context.drawn_nodes definition.id with
          | false -> Some (Edge.Child (definition, mode))
          | true ->
            Some
              (Edge.Shared
                 { id = definition.id; node = Some definition; mode })))
  in
  (* The block is the whole story: it names every field the walk kept, in
     order, and a field the walk descended through reads [Child] and takes
     [children]'s next node. Labeled edges come out in that order; anything
     the summary should not print (a slot drawn as an edge) lands in
     [hidden]. *)
  let edges, hidden =
    List.fold
      node.block
      ~init:([], [])
      ~f:(fun (edges, hidden) (label, block) ->
        let claim edge =
          match edge with
          | Some edge -> (label, edge) :: edges, label :: hidden
          | None -> edges, hidden
        in
        match block, role label with
        | Child, (`Interior | `Payload) -> claim (take_child (mode_at label))
        | Id (_ : int), (`Interior | `Payload) ->
          claim (claim_reference block ~mode:(mode_at label))
        (* the empty pointer: [Empty], [Nil], the end of a chain. A slot of a
           bucket array or a ring buffer holds them by the hundred, so there
           it stays silent rather than drawing a card's worth of nothing. *)
        | Int 0, `Interior ->
          (match shape with
           | Some (Shape.Elements _) -> edges, label :: hidden
           | Some (Shape.Fields _) | None -> claim (Some Edge.Nil))
        | Int 0, `Payload ->
          (match shape with
           | Some (Shape.Elements _) -> edges, label :: hidden
           | Some (Shape.Fields _) | None -> edges, hidden)
        | ( ( Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
            | Float_array _ | Address _ )
          , (`Interior | `Payload) ) ->
          edges, hidden)
  in
  (* a dump whose children outnumber its [Child] markers: draw the rest
     rather than drop them silently *)
  let unclaimed =
    List.map (Queue.to_list children) ~f:(fun child ->
      "", Edge.Child (child, Mode.Payload))
  in
  List.rev edges @ unclaimed, hidden
;;

(* past this many, an array says how big it is instead of what is in it *)
let max_inline_elements = 6

(* what the card says: [key → data] where the node holds a pair of payload
   fields, [length n]/[size n] for counters, the bare value for a single one,
   [x=1 y=2] for a record of the program's own, joined positions for
   anonymous user data *)
let summary_spans (node : Snapshot.Node.t) ~(mode : Mode.t) ~hidden_labels =
  (* [x=1 y=2] — one space between fields, none trailing, so the card is no
     wider than what it holds *)
  let named_fields fields =
    List.concat_mapi fields ~f:(fun index (label, value) ->
      let lead = match index with 0 -> "" | _ -> " " in
      [ `Label, [%string "%{lead}%{label}="]; `Value, value ])
  in
  let hidden label = List.mem hidden_labels label ~equal:String.equal in
  let visible =
    List.filter node.block ~f:(fun (label, (_ : Snapshot.Block.t)) ->
      not (hidden label))
  in
  let display (label, block) = label, Snapshot.Block.display block in
  match Mode.shape mode ~node with
  | Some (Shape.Fields { payload; interior = _ }) ->
    let is_payload label = List.mem payload label ~equal:String.equal in
    let payload_fields =
      List.filter visible ~f:(fun (label, (_ : Snapshot.Block.t)) ->
        is_payload label)
      |> List.map ~f:display
    in
    let leftovers =
      List.filter visible ~f:(fun (label, (_ : Snapshot.Block.t)) ->
        not (is_payload label))
      |> List.map ~f:display
      |> List.concat_map ~f:(fun (label, value) ->
        [ `Label, [%string "  %{label}="]; `Value, value ])
    in
    (* a catalogued structure's own field names are ours, not the program's,
       and mean nothing to the reader ([v], [d], [c]); a value of a declared
       type is the opposite — the names are the whole point *)
    let is_declared =
      match mode with
      | Mode.Ds User -> true
      | Mode.Ds
          ( Map | Set | Queue | Hashtbl | Stack | Dynarray | Core_map
          | Core_set | Core_hashtbl | Core_hash_set | Core_queue | Core_stack
          | Core_deque | Core_fdeque | Core_doubly_linked | Core_hash_queue
            )
      | Mode.Payload ->
        false
    in
    let main =
      match payload_fields, is_declared with
      | [], (true | false) -> [ `Arrow, "·" ]
      | fields, true -> named_fields fields
      (* the binding a keyed structure holds — a map node's [v]/[d], a
         bucket's [key]/[data], a Core hashtable's [k]/[v] *)
      | [ ("v", key); ("d", data) ], false
      | [ ("key", key); ("data", data) ], false
      | [ ("k", key); ("v", data) ], false ->
        [ `Key, key; `Arrow, " → "; `Value, data ]
      | [ ((("length" | "size" | "len") as label), value) ], false ->
        [ `Label, [%string "%{label} "]; `Value, value ]
      | [ ((_ : string), value) ], false -> [ `Value, value ]
      | fields, false -> named_fields fields
    in
    main @ leftovers
  (* A bucket array is mostly empty and its slots are the structure's own
     plumbing, so its size is all there is to say. A ring buffer or a dynamic
     array holds the user's data directly, so say what is in it — up to the
     point where a card would be wider than the pane. *)
  | Some (Shape.Elements { interior }) ->
    let slots =
      [ `Label, "slots "; `Value, Int.to_string (List.length node.block) ]
    in
    (match interior, visible with
     | true, (_ : (string * Snapshot.Block.t) list) -> slots
     | false, [] -> slots
     | false, fields ->
       (match List.length fields > max_inline_elements with
        | true -> slots
        | false ->
          [ ( `Value
            , List.map fields ~f:(fun ((_ : string), block) ->
                Snapshot.Block.display block)
              |> String.concat ~sep:", " )
          ]))
  (* generic user data: a tuple or an array labels its fields by position and
     reads better as bare values, a record of the program's own keeps the
     names the compiler's schema gave it *)
  | None ->
    let is_positional =
      List.for_all node.block ~f:(fun (label, (_ : Snapshot.Block.t)) ->
        String.for_all label ~f:Char.is_digit)
    in
    (match visible, is_positional with
     | [], (true | false) -> [ `Arrow, "·" ]
     | fields, true ->
       [ ( `Value
         , List.map fields ~f:(fun ((_ : string), block) ->
             Snapshot.Block.display block)
           |> String.concat ~sep:", " )
       ]
     | fields, false -> named_fields (List.map fields ~f:display))
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
  ~site
  ~fold_glyph
  ~hidden_count
  =
  let is_new = Set.mem context.new_addresses node.virtual_address in
  let root_structure = Map.find context.by_address node.virtual_address in
  let mark =
    Selection.mark context.selection ~address:node.virtual_address ~site
  in
  let border =
    Theme.fg'
      (match (mark : Selection.Mark.t) with
       | Cursor | Linked_to_cursor -> Theme.cursor
       | Selected | Linked_to_selected -> Theme.highlight
       | Plain -> Theme.card_border)
  in
  let summary =
    View.hcat
      (List.map (summary_spans node ~mode ~hidden_labels) ~f:span_view)
  in
  (* both picked-out cards spell their address out: the blue one because it
     is the chosen one, the orange one because knowing what you are about to
     choose is the point of aiming. A card merely linked to one of them does
     not — it is not where you are, and a second wide card would move the
     drawing for nothing. *)
  let address =
    match (mark : Selection.Mark.t) with
    | Linked_to_selected | Linked_to_cursor | Plain -> None
    | Selected | Cursor ->
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
        match (mark : Selection.Mark.t) with
        | Selected | Cursor -> [ Theme.fg Theme.text; Attr.bold ]
        | Linked_to_selected | Linked_to_cursor | Plain ->
          Theme.fg' Theme.text
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
    match (mark : Selection.Mark.t) with
    | Cursor ->
      View.with_colors' ~fill_backdrop:true ~bg:Theme.cursor_bg card
    | Selected ->
      View.with_colors' ~fill_backdrop:true ~bg:Theme.highlight_bg card
    | Linked_to_selected | Linked_to_cursor | Plain -> card
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

(* what the card over there is showing, so a pointer at it names the same
   thing it does. Interior edges are that card's business, not the pointer's,
   so only the payload survives — for a map node, its binding. *)
let shared_spans (node : Snapshot.Node.t) ~(mode : Mode.t) =
  let hidden_labels =
    match Mode.shape mode ~node with
    | Some (Shape.Fields { interior; payload }) ->
      List.filter_map node.block ~f:(fun (label, (_ : Snapshot.Block.t)) ->
        match
          List.mem interior label ~equal:String.equal
          || not (List.mem payload label ~equal:String.equal)
        with
        | true -> Some label
        | false -> None)
    | Some (Shape.Elements { interior = _ }) | None -> []
  in
  summary_spans node ~mode ~hidden_labels
;;

(* A card the canvas has already drawn, pointed at rather than drawn twice —
   which is the whole point of a persistent structure: [bigger] and [m] share
   two subtrees, and redrawing them would bury the one fact worth seeing.

   The pointer is a card too, dashed to say it is not the original, and it
   names its target by what that card holds rather than by the wire's node
   number: [↗ "b" → 2] names something on screen, [↗ #11] names nothing. It
   answers to the target's address, so choosing or aiming at either the
   pointer or the card lights up both, in blue or orange to match.
   {v
   ┌┄ ↗ ┄┄┄┄┄┄┄┐
   ┆ "b" → 2   ┆
   └┄┄┄┄┄┄┄┄┄┄┄┘
   v} *)
let shared_box
  (target : Snapshot.Node.t option)
  ~id
  ~mode
  ~(context : Context.t)
  ~site
  =
  let mark =
    match target with
    | None -> Selection.Mark.Plain
    | Some (node : Snapshot.Node.t) ->
      Selection.mark context.selection ~address:node.virtual_address ~site
  in
  let border =
    Theme.fg'
      (match (mark : Selection.Mark.t) with
       | Cursor | Linked_to_cursor -> Theme.cursor
       | Selected | Linked_to_selected -> Theme.highlight
       | Plain -> Theme.ghost)
  in
  let summary =
    match target with
    | Some node ->
      View.hcat (List.map (shared_spans node ~mode) ~f:span_view)
    | None -> View.text ~attrs:(Theme.fg' Theme.muted) [%string "#%{id#Int}"]
  in
  (* the arrow rides the border where a card's name does, and reads like one
     where the pointer is picked out *)
  let arrow =
    let attrs =
      match (mark : Selection.Mark.t) with
      | Selected | Cursor -> [ Theme.fg Theme.text; Attr.bold ]
      | Linked_to_selected | Linked_to_cursor | Plain ->
        Theme.fg' Theme.muted
    in
    View.text ~attrs " ↗ "
  in
  (* a picked pointer spells the address out for the same reason a picked
     card does — and here it is the whole argument: two boxes wearing one
     address are one object drawn twice *)
  let address =
    match (mark : Selection.Mark.t) with
    | Linked_to_selected | Linked_to_cursor | Plain -> None
    | Selected | Cursor ->
      Option.map target ~f:(fun (node : Snapshot.Node.t) ->
        View.text
          ~attrs:(Theme.fg' Theme.secondary)
          (Snapshot.Address.display node.virtual_address))
  in
  let inner =
    List.reduce_exn
      ~f:Int.max
      (View.width summary
       :: (View.width arrow - 2)
       :: List.map (Option.to_list address) ~f:View.width)
  in
  let content line =
    View.hcat
      [ View.text ~attrs:border "┆ "
      ; Panel.fit line ~width:inner ~height:1
      ; View.text ~attrs:border " ┆"
      ]
  in
  let rows =
    [ View.hcat
        [ View.text ~attrs:border "┌"
        ; arrow
        ; View.text
            ~attrs:border
            (Panel.repeat "┄" ~width:(inner + 2 - View.width arrow))
        ; View.text ~attrs:border "┐"
        ]
    ; content summary
    ]
    @ List.map (Option.to_list address) ~f:content
    @ [ View.text
          ~attrs:border
          [%string "└%{Panel.repeat \"┄\" ~width:(inner + 2)}┘"]
      ]
  in
  let card =
    Panel.fit (View.vcat rows) ~width:(inner + 4) ~height:(List.length rows)
  in
  match (mark : Selection.Mark.t) with
  | Cursor -> View.with_colors' ~fill_backdrop:true ~bg:Theme.cursor_bg card
  | Selected ->
    View.with_colors' ~fill_backdrop:true ~bg:Theme.highlight_bg card
  | Linked_to_selected | Linked_to_cursor | Plain -> card
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
  ~depth
  ~parent
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
  let site = { Site.structure = structure_id; path = List.rev path } in
  let fold = Fold.Node (structure_id, site.path) in
  let collapsible = not (List.is_empty edges) in
  let folded = collapsible && Set.mem folds fold in
  let fold_glyph =
    match collapsible with true -> Some folded | false -> None
  in
  let leaf_box ~hidden_count =
    node_box
      node
      ~mode
      ~hidden_labels
      ~context
      ~site
      ~fold_glyph
      ~hidden_count
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
        ; site
        ; depth
        ; parent
        ; is_pointer = false
        }
      ]
    , [] )
  | edges ->
    let shape = Mode.shape mode ~node in
    let rendered =
      List.mapi edges ~f:(fun index (label, edge) ->
        let label = pretty_label ~shape label in
        match edge with
        | Edge.Nil -> label, (nil_box, View.width nil_box / 2, [], [])
        | Edge.Shared { id; node = target; mode } ->
          (* the node is on the canvas already — point at it, wearing its
             address so that picking either end lights both up. The pointer
             keeps a site of its own, at the edge it hangs off, so the cursor
             standing here stays here. *)
          let stub_site =
            { Site.structure = structure_id
            ; path = List.rev (index :: path)
            }
          in
          let stub = shared_box target ~id ~mode ~context ~site:stub_site in
          let placed =
            match target with
            | None -> []
            | Some (target : Snapshot.Node.t) ->
              [ { Placed.x = 0
                ; y = 0
                ; width = View.width stub
                ; height = View.height stub
                ; address = target.virtual_address
                ; site = stub_site
                ; depth = depth + 1
                ; parent = Some site
                ; is_pointer = true
                }
              ]
          in
          label, (stub, View.width stub / 2, placed, [])
        | Edge.Child (child, mode) ->
          ( label
          , tree
              child
              ~ds_type
              ~mode
              ~context
              ~folds
              ~structure_id
              ~path:(index :: path)
              ~depth:(depth + 1)
              ~parent:(Some site) )
        | Edge.Ref (structure : Replay.Structure.t) ->
          ( label
          , tree
              structure.snapshot.root_node
              ~ds_type:structure.snapshot.ds_type
              ~mode:(Mode.Ds structure.snapshot.ds_type)
              ~context
              ~folds
              ~structure_id:structure.id
              ~path:[]
              ~depth:(depth + 1)
              ~parent:(Some site) ))
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
      ; site
      ; depth
      ; parent
      ; is_pointer = false
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

(* one structure's block — its header row with its tree under it — measured
   and positioned relative to its own top-left corner, so {!pack} can put it
   anywhere *)
module Section = struct
  type t =
    { view : View.t
    ; width : int
    ; height : int
    ; placed : Placed.t list
    ; toggles : Toggle.t list
    }
end

(* Every live structure, in registry order: a header, and its tree unless the
   header's fold hides it. A structure referenced from another one is drawn
   inside its referrer's tree instead of getting a section — the registry
   still decides what is alive, only the placement moves. *)
let sections ~structures ~nodes ~new_addresses ~folds ~selection =
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
  let section (sections : Section.t list) (structure : Replay.Structure.t) =
    match Hash_set.mem context.drawn structure.id with
    | true -> sections
    | false ->
      Hash_set.add context.drawn structure.id;
      let folded = Set.mem folds (Fold.Structure structure.id) in
      let header = structure_header structure ~folded in
      let header_toggle =
        { Toggle.x = 0; y = 0; fold = Fold.Structure structure.id }
      in
      (* the tree lays out either way so its reference claims hold — a folded
         structure keeps what it references hidden with it *)
      let canvas, (_ : int), placed, toggles =
        tree
          (Replay.Structure.current_root structure)
          ~ds_type:structure.snapshot.ds_type
          ~mode:(Mode.Ds structure.snapshot.ds_type)
          ~context
          ~folds
          ~structure_id:structure.id
          ~path:[]
          ~depth:0
          ~parent:None
      in
      let block =
        match folded with
        | true ->
          (* the header is the whole summary *)
          { Section.view = header
          ; width = View.width header
          ; height = 1
          ; placed = []
          ; toggles = [ header_toggle ]
          }
        | false ->
          { Section.view = View.zcat [ View.pad ~t:1 canvas; header ]
          ; width = Int.max (View.width header) (View.width canvas)
          ; height = 1 + View.height canvas
          ; placed = List.map placed ~f:(Placed.shift ~dx:0 ~dy:1)
          ; toggles =
              header_toggle :: List.map toggles ~f:(Toggle.shift ~dx:0 ~dy:1)
          }
      in
      block :: sections
  in
  let top_level =
    List.filter structures ~f:(fun (structure : Replay.Structure.t) ->
      not (Set.mem referenced structure.id))
  in
  let acc = List.fold top_level ~init:[] ~f:section in
  (* mutually-referencing structures have no unreferenced root; anything
     still undrawn gets its own section after all *)
  List.rev (List.fold structures ~init:acc ~f:section)
;;

let columns = 3
let column_gap = 3
let row_gap = 1

(* Sections lay side by side rather than stacking in one column. A run
   allocates many small structures and a few large ones, and a single column
   spent most of a pane — now two thirds of the screen — on nothing: a map
   beside the queue holding it beside the version one more [add] returned is
   the comparison the pane exists to make.

   They flow in registry order, up to [columns] to a row, and a section that
   would not fit in what is left of a row starts the next one — so the
   columns are as wide as the trees in them, and a tree wider than the pane
   still gets a row to itself rather than being squeezed. Each row is as tall
   as its tallest section. *)
let pack sections ~body_width =
  let place (views, all_placed, all_toggles, row) (section : Section.t) =
    let x, y, row_height, count = row in
    let starts_row =
      count > 0 && (count >= columns || x + section.width > body_width)
    in
    let x, y, row_height, count =
      match starts_row with
      | true -> 0, y + row_height + row_gap, 0, 0
      | false -> x, y, row_height, count
    in
    ( View.pad ~l:x ~t:y section.view :: views
    , List.map section.placed ~f:(Placed.shift ~dx:x ~dy:y) @ all_placed
    , List.map section.toggles ~f:(Toggle.shift ~dx:x ~dy:y) @ all_toggles
    , ( x + section.width + column_gap
      , y
      , Int.max row_height section.height
      , count + 1 ) )
  in
  let views, placed, toggles, (_ : int * int * int * int) =
    List.fold sections ~init:([], [], [], (0, 0, 0, 0)) ~f:place
  in
  View.zcat views, placed, toggles
;;

let layout ~structures ~nodes ~new_addresses ~folds ~selection ~body_width =
  pack
    (sections ~structures ~nodes ~new_addresses ~folds ~selection)
    ~body_width
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
  | Some { Spot.site; address = (_ : Snapshot.Address.t) } ->
    (match card_at placed site with
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
    layout
      ~structures
      ~nodes
      ~new_addresses
      ~folds
      ~selection
      ~body_width:(Panel.inner_width ~width)
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
  ~width
  ~height
  ~x
  ~y
  =
  let canvas, placed, toggles =
    layout
      ~structures
      ~nodes
      ~new_addresses
      ~folds
      ~selection
      ~body_width:(Panel.inner_width ~width)
  in
  let scroll = resolve_scroll canvas placed ~height ~scroll ~selection in
  let y = y + scroll in
  List.find toggles ~f:(fun (toggle : Toggle.t) ->
    x = toggle.x && y = toggle.y)
  |> Option.map ~f:(fun (toggle : Toggle.t) -> toggle.fold)
;;

let spot_at
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~scroll
  ~selection
  ~width
  ~height
  ~x
  ~y
  =
  let canvas, placed, (_ : Toggle.t list) =
    layout
      ~structures
      ~nodes
      ~new_addresses
      ~folds
      ~selection
      ~body_width:(Panel.inner_width ~width)
  in
  let scroll = resolve_scroll canvas placed ~height ~scroll ~selection in
  List.find placed ~f:(Placed.contains ~x ~y:(y + scroll))
  |> Option.map ~f:Placed.spot
;;

(* where the pane starts you off: a structure's own root card *)
let spot_of_structure (structure : Replay.Structure.t) =
  { Spot.address = structure.address
  ; site = { Site.structure = structure.id; path = [] }
  }
;;

(* the cards as the sections built them — before packing, so their positions
   are relative to their own structure. That is all the cursor reads: a
   card's depth, its parent, and its column within its own tree. Where the
   pane later drops that tree cannot change where [d] lands. *)
let cards ~structures ~nodes ~new_addresses ~folds ~selection =
  sections ~structures ~nodes ~new_addresses ~folds ~selection
  |> List.concat_map ~f:(fun (section : Section.t) -> section.placed)
;;

(* A spot picked at one step can name a card another step does not draw:
   [Enter] on a [↗] pointer jumps the replay to where that node was
   allocated, and the structure the pointer lived in need not have existed
   back there. The node itself usually survives the jump, so re-point the
   spot at whatever card draws it now — [None] only when nothing on the
   canvas is that node at all, and the pane falls back to the walked
   structure. *)
let resolve_spot
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ({ Spot.address; site } as spot)
  =
  let placed =
    cards ~structures ~nodes ~new_addresses ~folds ~selection:Selection.none
  in
  match card_at placed site with
  | Some (_ : Placed.t) -> Some spot
  | None ->
    (* the node's own card if it has one, since that is where it lives; a
       pointer at it only if nothing else on the canvas draws it *)
    let drawings =
      List.filter placed ~f:(fun (card : Placed.t) ->
        Snapshot.Address.equal card.address address)
    in
    (match
       List.find drawings ~f:(fun (card : Placed.t) -> not card.is_pointer)
     with
     | Some card -> Some (Placed.spot card)
     | None -> List.hd drawings |> Option.map ~f:Placed.spot)
;;

(* The cursor walks the tree, not the picture: [w]/[s] climb and descend it,
   [a]/[d] run along a layer. Reading the diagram's own structure rather than
   card positions means aiming stays put even though the drawing shifts a
   little as cards gain and lose their address line — and it means [d] from a
   left child reaches its cousin on the right instead of whatever card
   happens to be nearest.

   Empty slots place no card, so a layer skips over them; a [↗] pointer does
   place one, wearing the address of the card it points at, so aiming at a
   pointer aims at that card wherever the pane drew it.

   Structures are a layer of their own — the outermost one. From a root,
   [a]/[d] step to the structure beside it and [w] to the one before it; [s]
   off a leaf falls through to the structure after. *)
let move_cursor
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~selection
  ~(direction : Direction.t)
  =
  let placed = cards ~structures ~nodes ~new_addresses ~folds ~selection in
  let find site = card_at placed site in
  let by_x = List.sort ~compare:(fun (a : Placed.t) b -> compare a.x b.x) in
  (* registry order, which is also the order the columns are filled in *)
  let roots =
    List.filter placed ~f:(fun (card : Placed.t) ->
      Option.is_none card.parent)
  in
  let rec root_of (card : Placed.t) =
    match Option.bind card.parent ~f:find with
    | None -> card
    | Some parent -> root_of parent
  in
  (* the tree above or below this one, for the climb off either end *)
  let sibling_tree (card : Placed.t) ~offset =
    let root = root_of card in
    match
      List.findi roots ~f:(fun (_ : int) (other : Placed.t) ->
        Site.equal other.site root.site)
    with
    | None -> None
    | Some (index, (_ : Placed.t)) -> List.nth roots (index + offset)
  in
  let neighbour_in list (card : Placed.t) ~offset =
    match
      List.findi list ~f:(fun (_ : int) (other : Placed.t) ->
        Site.equal other.site card.site)
    with
    | None -> None
    | Some (index, (_ : Placed.t)) -> List.nth list (index + offset)
  in
  let from =
    Option.first_some selection.cursor selection.selected
    |> Option.bind ~f:(fun (spot : Spot.t) -> find spot.site)
  in
  match from with
  | None ->
    (* nothing aimed at yet: start at the first tree's root *)
    List.hd roots |> Option.map ~f:Placed.spot
  | Some card ->
    let moved =
      match direction with
      | Up ->
        (match Option.bind card.parent ~f:find with
         | Some parent -> Some parent
         | None -> sibling_tree card ~offset:(-1))
      | Down ->
        let children =
          List.filter placed ~f:(fun (other : Placed.t) ->
            match other.parent with
            | Some parent -> Site.equal parent card.site
            | None -> false)
          |> by_x
        in
        (match children with
         | first :: (_ : Placed.t list) -> Some first
         | [] -> sibling_tree card ~offset:1)
      | Left | Right ->
        let offset =
          match direction with Left -> -1 | Up | Down | Right -> 1
        in
        (match card.parent with
         (* a root has no layer inside its own tree; the structures beside it
            are its layer, and the pane tiles them left to right *)
         | None -> sibling_tree card ~offset
         | Some (_ : Site.t) ->
           let root = root_of card in
           let layer =
             List.filter placed ~f:(fun (other : Placed.t) ->
               other.depth = card.depth
               && Site.equal (root_of other).site root.site)
             |> by_x
           in
           neighbour_in layer card ~offset)
    in
    Option.map moved ~f:Placed.spot
;;
