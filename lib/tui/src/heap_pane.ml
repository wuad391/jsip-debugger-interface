open! Core
open Jsip_types
open Jsip_replay
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

(* Which drawing of a node the keyboard is standing on. A node can be on the
   canvas twice — its own card, and a [↗] pointer at it from a structure that
   shares it — and those are two places even though they are one object, so a
   position has to say which one it means. Keyed like a fold: the owning
   structure, and the edge path down to the card. *)
module Site = struct
  type t =
    { structure : int
    ; path : int list
    ; is_header : bool
    (** the section's [name · kind] line rather than a card in its tree. A
        collapsed structure is nothing but its header, so without somewhere
        to stand on it there would be no way back — you could fold a
        structure away and never reach it again. *)
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

(* one node's outgoing edges and inline fields, straight off the wire *)
module Edge = struct
  (* a node already drawn elsewhere on this canvas: the wire shares it, so
     the canvas points at it rather than drawing it twice. The pointer
     carries the node the id defines, so it can name what it points at the
     way that card does. *)
  type shared =
    { id : int
    ; node : Snapshot.Node.t option
    }

  type t =
    | Nil (** an empty interior slot *)
    | Child of Snapshot.Node.t
    | Ref of Replay.Structure.t
    (** a tracked structure reached through a reference *)
    | Shared of shared
end

(* Read a node the way the wire writes it: [block] holds every kept field
   under its own label and in field order, a field holding a walked block
   reads [Child] and stands for the next node in [children], and an [Id]
   names a node defined earlier. Nothing here needs a layout — the only
   per-structure knowledge left is {!Snapshot.Ds_type.interior_labels}, which
   says which [Int 0] is an empty pointer rather than the number.

   Returns the labeled edges in field order, and the leaf fields the card
   should print. *)
let node_edges (node : Snapshot.Node.t) ~ds_type ~(context : Context.t) =
  let children = Queue.of_list node.children in
  let interior = Snapshot.Ds_type.interior_labels ds_type in
  let is_interior label = List.mem interior label ~equal:String.equal in
  (* An [Id] names a node the dump defined earlier. If it is a tracked
     structure's root, its whole tree links in here; otherwise it is a shared
     block, which draws here the first time and points back every time after. *)
  let claim_reference block =
    match Context.structure context block with
    | Some (structure : Replay.Structure.t) ->
      (match Hash_set.mem context.drawn structure.id with
       | false ->
         Hash_set.add context.drawn structure.id;
         Some (Edge.Ref structure)
       | true ->
         Some
           (Edge.Shared
              { id = structure.id; node = Some structure.snapshot.root_node }))
    | None ->
      (match Context.node context block with
       | None -> None
       | Some (definition : Snapshot.Node.t) ->
         (match Hash_set.mem context.drawn_nodes definition.id with
          | false -> Some (Edge.Child definition)
          | true ->
            Some (Edge.Shared { id = definition.id; node = Some definition })))
  in
  let edges, leaves =
    List.fold
      node.block
      ~init:([], [])
      ~f:(fun (edges, leaves) (label, block) ->
        match (block : Snapshot.Block.t) with
        | Child ->
          (match Queue.dequeue children with
           | Some child -> (label, Edge.Child child) :: edges, leaves
           | None -> edges, leaves)
        | Id (_ : int) ->
          (match claim_reference block with
           | Some edge -> (label, edge) :: edges, leaves
           | None -> edges, (label, block) :: leaves)
        (* the empty pointer and the number nought are the same word; only
           the structure's own skeleton labels tell them apart *)
        | Int 0 when is_interior label -> (label, Edge.Nil) :: edges, leaves
        | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
        | Float_array _ | Address _ ->
          edges, (label, block) :: leaves)
  in
  (* a walked block the wire did not label; nothing emits these today, but
     drawing them beats dropping them *)
  let unclaimed =
    List.map (Queue.to_list children) ~f:(fun child -> "", Edge.Child child)
  in
  List.rev edges @ unclaimed, List.rev leaves
;;

(* the classic two-field payloads, by the labels the walker gives them: a map
   node's key and data, a hashtable entry's, a hash queue pair's *)
let is_binding = function
  | "v", "d" | "k", "v" | "key", "data" -> true
  | (_ : string), (_ : string) -> false
;;

let is_positional label = String.for_all label ~f:Char.is_digit

(* What the card says. [key → data] where the node holds one of the known
   binding pairs, [length n] for a counter, the bare value where there is
   only one, positional values joined where the labels are an array's or a
   tuple's, and [label=value] otherwise — user records included, which is why
   the arrow is not simply "any two fields".

   A positional field holding [Int 0] is dropped: an array's empty slots are
   [Int 0], and a bucket array is mostly empty slots, so printing them buries
   the one that is set under fifteen that are not. That does hide a literal
   zero sitting in a tuple — the same trade the pane has always made, now
   stated where it happens rather than hidden in a layer mask. A card left
   with nothing to say falls back to how many slots it has. *)
let summary_spans leaves ~arity =
  let kept =
    List.filter leaves ~f:(fun (label, block) ->
      match is_positional label, (block : Snapshot.Block.t) with
      | true, Int 0 -> false
      | (true | false), _ -> true)
  in
  let fields =
    List.map kept ~f:(fun (label, block) ->
      label, Snapshot.Block.display block)
  in
  let positional =
    (not (List.is_empty leaves))
    && List.for_all
         leaves
         ~f:(fun ((label : string), (_ : Snapshot.Block.t)) ->
           is_positional label)
  in
  match fields with
  | [] when positional -> [ `Label, "slots "; `Value, Int.to_string arity ]
  | [] -> [ `Arrow, "·" ]
  | fields when positional ->
    [ `Value, String.concat (List.map fields ~f:snd) ~sep:", " ]
  | [ (key_label, key); (data_label, data) ]
    when is_binding (key_label, data_label) ->
    [ `Key, key; `Arrow, " → "; `Value, data ]
  | [ ((("length" | "size" | "len" | "num_readers") as label), value) ] ->
    [ `Label, [%string "%{label} "]; `Value, value ]
  | [ ((_ : string), value) ] -> [ `Value, value ]
  | fields ->
    List.concat_mapi fields ~f:(fun index (label, value) ->
      let lead = match index with 0 -> "" | _ -> "  " in
      [ `Label, [%string "%{lead}%{label}="]; `Value, value ])
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

   Only the picked cards spell out their address, and it rides the BOTTOM
   border rather than taking a row of its own. A row would make every card
   the cursor touches a row taller, and card heights are what the tree and
   the columns are laid out from — so aiming would shuffle the whole canvas
   about. On the border it costs nothing vertical, and [show_address:false]
   measures the card as if it were not picked, which is what everything
   geometric uses.
   {v
   ┌ m ──────── new ┐
   │"a" → 2         │
   └ 0x763be65ee878 ┘   ← picked only
        ⋯ 3 hidden
   v} *)
let node_box
  (node : Snapshot.Node.t)
  ~leaves
  ~arity
  ~(context : Context.t)
  ~site
  ~fold_glyph
  ~hidden_count
  ~show_address
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
    View.hcat (List.map (summary_spans leaves ~arity) ~f:span_view)
  in
  (* both picked-out cards spell their address out: the blue one because it
     is the chosen one, the orange one because knowing what you are about to
     choose is the point of aiming. A card merely linked to one of them does
     not — it is not where you are. *)
  let address =
    match show_address, (mark : Selection.Mark.t) with
    | `Never, _
    | `When_picked, (Linked_to_selected | Linked_to_cursor | Plain) ->
      None
    | `Always, _ | `When_picked, (Selected | Cursor) ->
      Some
        (View.text
           ~attrs:(Theme.fg' Theme.secondary)
           [%string " %{Snapshot.Address.display node.virtual_address} "])
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
       :: riders_width
       :: List.map (Option.to_list address) ~f:View.width)
  in
  (* every row is exactly [inner + 2] cells, so the wash covers the card and
     nothing else *)
  let card_width = inner + 2 in
  let top =
    View.hcat
      [ View.text ~attrs:border "┌"
      ; name_tag
      ; View.text
          ~attrs:border
          (Panel.repeat "─" ~width:(inner - riders_width))
      ; new_tag
      ; View.text ~attrs:border "┐"
      ]
  in
  let bottom =
    match address with
    | None ->
      View.text
        ~attrs:border
        [%string "└%{Panel.repeat \"─\" ~width:inner}┘"]
    | Some address ->
      View.hcat
        [ View.text ~attrs:border "└"
        ; address
        ; View.text
            ~attrs:border
            (Panel.repeat "─" ~width:(inner - View.width address))
        ; View.text ~attrs:border "┘"
        ]
  in
  let content line =
    View.hcat
      [ View.text ~attrs:border "│"
      ; Panel.fit line ~width:inner ~height:1
      ; View.text ~attrs:border "│"
      ]
  in
  let rows = [ top; content summary; bottom ] in
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

(* What the card over there is showing, so a pointer at it names the same
   thing it does: the fields that are not edges, summarized the way that card
   summarizes them.

   Deliberately NOT {!node_edges} — that claims references as it goes, and
   this is a second look at a node someone else is drawing. Claiming here
   would mark a structure drawn on behalf of a card that never draws it, and
   the structure would vanish from the pane. *)
let shared_spans (node : Snapshot.Node.t) ~ds_type =
  let interior = Snapshot.Ds_type.interior_labels ds_type in
  let leaves =
    List.filter node.block ~f:(fun (label, block) ->
      match (block : Snapshot.Block.t) with
      | Child | Id (_ : int) -> false
      | Int 0 -> not (List.mem interior label ~equal:String.equal)
      | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
      | Float_array _ | Address _ ->
        true)
  in
  summary_spans leaves ~arity:(List.length node.block)
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
  ~ds_type
  ~(context : Context.t)
  ~site
  ~show_address
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
      View.hcat (List.map (shared_spans node ~ds_type) ~f:span_view)
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
    match show_address, (mark : Selection.Mark.t) with
    | `Never, _
    | `When_picked, (Linked_to_selected | Linked_to_cursor | Plain) ->
      None
    | `Always, _ | `When_picked, (Selected | Cursor) ->
      Option.map target ~f:(fun (node : Snapshot.Node.t) ->
        View.text
          ~attrs:(Theme.fg' Theme.secondary)
          [%string " %{Snapshot.Address.display node.virtual_address} "])
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
  ~(context : Context.t)
  ~folds
  ~structure_id
  ~path
  ~depth
  ~parent
  : View.t * int * int * Placed.t list * Toggle.t list
  =
  let edges, leaves = node_edges node ~ds_type ~context in
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
  let site =
    { Site.structure = structure_id
    ; path = List.rev path
    ; is_header = false
    }
  in
  let fold = Fold.Node (structure_id, site.path) in
  let collapsible = not (List.is_empty edges) in
  let folded = collapsible && Set.mem folds fold in
  let fold_glyph =
    match collapsible with true -> Some folded | false -> None
  in
  let leaf_box ~hidden_count ~show_address =
    node_box
      node
      ~leaves
      ~arity:(List.length node.block)
      ~context
      ~site
      ~fold_glyph
      ~hidden_count
      ~show_address
  in
  (* Geometry measures every card with room for an address, drawn or not. A
     card only shows one while it is picked, but reserving the space on all
     of them is what lets the cursor move without the diagram shuffling
     about: the card grows into space that was already set aside for it, and
     nothing beside it has to shift. Height never enters into it — the
     address rides the bottom border rather than taking a row. *)
  let reserved_box ~hidden_count =
    leaf_box ~hidden_count ~show_address:`Always
  in
  Hash_set.add context.drawn_nodes node.id;
  match edges with
  | [] ->
    let box = leaf_box ~hidden_count:0 ~show_address:`When_picked in
    let box_width = View.width (reserved_box ~hidden_count:0) in
    ( box
    , box_width / 2
    , box_width
    , [ { Placed.x = 0
        ; y = 0
        ; width = View.width box
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
    let rendered =
      List.mapi edges ~f:(fun index (label, edge) ->
        match edge with
        | Edge.Nil ->
          label, (nil_box, View.width nil_box / 2, View.width nil_box, [], [])
        | Edge.Shared { id; node = target } ->
          (* the node is on the canvas already — point at it, wearing its
             address so that picking either end lights both up. The pointer
             keeps a site of its own, at the edge it hangs off, so the cursor
             standing here stays here. *)
          let stub_site =
            { Site.structure = structure_id
            ; path = List.rev (index :: path)
            ; is_header = false
            }
          in
          let stub =
            shared_box
              target
              ~id
              ~ds_type
              ~context
              ~site:stub_site
              ~show_address:`When_picked
          in
          let plain =
            shared_box
              target
              ~id
              ~ds_type
              ~context
              ~site:stub_site
              ~show_address:`Always
          in
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
          label, (stub, View.width plain / 2, View.width plain, placed, [])
        | Edge.Child child ->
          ( label
          , tree
              child
              ~ds_type
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
        ~f:(fun x (label, (view, center, plain_width, placed, toggles)) ->
          ( x + plain_width + sibling_gap
          , (label, view, x, x + center, plain_width, placed, toggles) ))
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
              , (_ : int)
              , placed
              , (_ : Toggle.t list) )
            -> List.length placed)
    in
    Hash_set.add context.drawn_nodes node.id;
    let box = leaf_box ~hidden_count ~show_address:`When_picked in
    (* geometry always follows the unfolded, unpicked card, so neither
       folding (whose ⋯ tag can widen the border) nor aiming moves a center *)
    let box_width = View.width (reserved_box ~hidden_count:0) in
    let box_height = View.height (reserved_box ~hidden_count) in
    let centers =
      List.map
        placed_children
        ~f:
          (fun
            ( (_ : string)
            , (_ : View.t)
            , (_ : int)
            , center
            , (_ : int)
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
            ((_ : string), view, x, (_ : int), (_ : int), placed, toggles)
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
    (* the subtree's footprint, measured off the unpicked cards — what the
       parent spaces its siblings by, so aiming widens a card into the gap
       beside it rather than shoving the diagram along *)
    let plain_width =
      List.fold
        placed_children
        ~init:(parent_x + box_width)
        ~f:
          (fun
            widest
            ( (_ : string)
            , (_ : View.t)
            , x
            , (_ : int)
            , child_width
            , (_ : Placed.t list)
            , (_ : Toggle.t list) )
          -> Int.max widest (x + child_shift + child_width))
    in
    (match folded with
     | false ->
       ( expanded
       , parent_center
       , plain_width
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
           ; View.transparent_rectangle ~width:plain_width ~height:1
           ]
       in
       ( canvas
       , parent_center
       , plain_width
       , [ Placed.shift box_placed ~dx:folded_x ~dy:0 ]
       , List.map box_toggles ~f:(Toggle.shift ~dx:folded_x ~dy:0) ))
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

(* the name · kind · type line — what a structure's header says about it, and
   so also what [/] lets you filter by *)
let header_text (structure : Replay.Structure.t) =
  let label =
    [%string
      "%{Replay.Structure.display structure} · %{Snapshot.Ds_type.display \
       structure.snapshot.ds_type}"]
  in
  match structure.ty with
  | None -> label
  | Some ty -> [%string "%{label} %{Type_info.display ty}"]
;;

let matches_filter structure ~filter =
  match String.is_empty filter with
  | true -> true
  | false ->
    String.is_substring
      (String.lowercase (header_text structure))
      ~substring:(String.lowercase filter)
;;

(* the section header over one structure's tree: a fold glyph, then its name
   (or [#id]), kind and size — which is exactly the summary a folded
   structure collapses to. The one this step's event walked reads in the
   highlight blue. *)
let structure_header (structure : Replay.Structure.t) ~folded ~mark =
  (* size on the summary line: it is all you see of a collapsed structure,
     and how big each one is is the thing worth scanning for when hundreds of
     them are collapsed *)
  let size =
    match count_nodes [ structure ] with
    | 1 -> "1 node"
    | count -> [%string "%{count#Int} nodes"]
  in
  let label = [%string "%{header_text structure} · %{size}"] in
  (* standing on the header means the whole structure is what is picked out,
     so it takes the same colours a picked card does. Only an exact match
     counts: the root card shares this address, and a card is not its
     structure. *)
  let label_attrs =
    match (mark : Selection.Mark.t) with
    | Cursor -> [ Theme.fg Theme.cursor_deep; Attr.bold ]
    | Selected -> [ Theme.fg Theme.highlight_deep; Attr.bold ]
    | Linked_to_selected | Linked_to_cursor | Plain ->
      (match structure.is_current with
       | true -> [ Theme.fg Theme.highlight_deep; Attr.bold ]
       | false -> Theme.fg' Theme.muted)
  in
  let line =
    View.hcat
      [ View.text ~attrs:(Theme.fg' Theme.secondary) (glyph_of ~folded)
      ; View.text " "
      ; View.text ~attrs:label_attrs label
      ]
  in
  match (mark : Selection.Mark.t) with
  | Cursor -> View.with_colors' ~fill_backdrop:true ~bg:Theme.cursor_bg line
  | Selected ->
    View.with_colors' ~fill_backdrop:true ~bg:Theme.highlight_bg line
  | Linked_to_selected | Linked_to_cursor | Plain -> line
;;

(* one structure's block — its header row with its tree under it — measured
   and positioned relative to its own top-left corner, so {!pack} can put it
   anywhere *)
module Section = struct
  type t =
    { view : View.t
    ; height : int (** as drawn, folds and all *)
    ; reserved_width : int
    ; reserved_height : int
    (** the footprint with the structure expanded — what {!pack} chooses a
        column from, so collapsing one does not move the others sideways *)
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
      (* the header is the structure itself, one rung above its root card:
         [w] off the root reaches it, and a collapsed structure is nothing
         BUT its header, which is how you get back into one *)
      let header_site =
        { Site.structure = structure.id; path = []; is_header = true }
      in
      let header =
        structure_header
          structure
          ~folded
          ~mark:
            (Selection.mark
               selection
               ~address:structure.address
               ~site:header_site)
      in
      let header_placed =
        { Placed.x = 0
        ; y = 0
        ; width = View.width header
        ; height = 1
        ; address = structure.address
        ; site = header_site
        ; depth = 0
        ; parent = None
        ; is_pointer = false
        }
      in
      let header_toggle =
        { Toggle.x = 0; y = 0; fold = Fold.Structure structure.id }
      in
      (* the tree lays out either way so its reference claims hold — a folded
         structure keeps what it references hidden with it *)
      let canvas, (_ : int), plain_width, placed, toggles =
        tree
          (Replay.Structure.current_root structure)
          ~ds_type:structure.snapshot.ds_type
          ~context
          ~folds
          ~structure_id:structure.id
          ~path:[]
          ~depth:1
          ~parent:(Some header_site)
      in
      (* the tree is laid out either way, so its footprint is known even when
         the header is hiding it *)
      let reserved_width = Int.max (View.width header) plain_width in
      let reserved_height = 1 + View.height canvas in
      let block =
        match folded with
        | true ->
          (* the header is the whole summary *)
          { Section.view = header
          ; height = 1
          ; reserved_width
          ; reserved_height
          ; placed = [ header_placed ]
          ; toggles = [ header_toggle ]
          }
        | false ->
          { Section.view = View.zcat [ View.pad ~t:1 canvas; header ]
          ; reserved_width
          ; reserved_height
          ; height = 1 + View.height canvas
          ; placed =
              header_placed :: List.map placed ~f:(Placed.shift ~dx:0 ~dy:1)
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

   They pack bottom-left against a skyline rather than into rows or into
   fixed columns. Rows were as tall as their tallest member, which on a dump
   with a thousand structures left more blank canvas than diagram; fixed
   columns made a section a shade too wide claim two of them and waste the
   rest. A section takes the width it actually needs, floored at a third of
   the pane so no more than three ever sit abreast, and drops into the
   highest place it fits.

   x is chosen against the RESERVED skyline — heights as if nothing were
   folded — so collapsing a structure moves it up its column without moving
   anything sideways. y then comes from the real one, so the space a collapse
   frees is actually freed. *)
let pack sections ~body_width =
  let body_width = Int.max 1 body_width in
  let column_width =
    Int.max 1 ((body_width - ((columns - 1) * column_gap)) / columns)
  in
  let slot (section : Section.t) =
    Int.min body_width (Int.max column_width section.reserved_width)
    + column_gap
  in
  (* height of the tallest thing already occupying [x, x + width) *)
  let ceiling skyline ~x ~width =
    let stop = Int.min body_width (x + width) in
    List.range x stop
    |> List.fold ~init:0 ~f:(fun tallest at -> Int.max tallest skyline.(at))
  in
  let raise_to skyline ~x ~width ~y =
    let stop = Int.min body_width (x + width) in
    List.range x stop |> List.iter ~f:(fun at -> skyline.(at) <- y)
  in
  (* the highest place a run of [width] fits, leftmost among ties *)
  let settle skyline ~width =
    let last = Int.max 0 (body_width - width) in
    List.range 0 (last + 1)
    |> List.map ~f:(fun x -> ceiling skyline ~x ~width, x)
    |> List.min_elt ~compare:[%compare: int * int]
    |> Option.value ~default:(0, 0)
  in
  (* pass one places against reserved heights and keeps only the column it
     chose; pass two drops each section into that column at its real height *)
  let reserved = Array.create ~len:body_width 0 in
  let columns_chosen =
    List.map sections ~f:(fun (section : Section.t) ->
      let width = slot section in
      let y, x = settle reserved ~width in
      raise_to reserved ~x ~width ~y:(y + section.reserved_height + row_gap);
      x)
  in
  let skyline = Array.create ~len:body_width 0 in
  let place (views, all_placed, all_toggles) ((section : Section.t), x) =
    let width = slot section in
    let y = ceiling skyline ~x ~width in
    raise_to skyline ~x ~width ~y:(y + section.height + row_gap);
    ( View.pad ~l:x ~t:y section.view :: views
    , List.map section.placed ~f:(Placed.shift ~dx:x ~dy:y) @ all_placed
    , List.map section.toggles ~f:(Toggle.shift ~dx:x ~dy:y) @ all_toggles )
  in
  let views, placed, toggles =
    List.fold
      (List.zip_exn sections columns_chosen)
      ~init:([], [], [])
      ~f:place
  in
  View.zcat views, placed, toggles
;;

let layout ~structures ~nodes ~new_addresses ~folds ~selection ~body_width =
  pack
    (sections ~structures ~nodes ~new_addresses ~folds ~selection)
    ~body_width
;;

(* Bring one span into a window of [size], from an offset of [at].

   A card just past the edge takes the smallest adjustment that shows it, so
   scrolling by hand and then stepping the cursor does not throw the view
   somewhere else. A card nowhere near the window is a different matter —
   that is a jump to another structure, and edge-aligning it puts the thing
   you asked for against the frame with its surroundings all on one side, so
   those get centred. *)
let bring_into_view ~at ~size ~start ~length =
  match start + length <= at || start >= at + size with
  | true -> start + (length / 2) - (size / 2)
  | false -> Int.max (start + length - size) (Int.min at start)
;;

(* The wheel and PgUp/PgDn set the scroll, but the cursor overrides it: a
   card you cannot see is a card you cannot aim at. *)
let follow_cursor placed ~body_height ~scroll ~(selection : Selection.t) =
  match selection.cursor with
  | None -> scroll
  | Some { Spot.site; address = (_ : Snapshot.Address.t) } ->
    (match card_at placed site with
     | None -> scroll
     | Some card ->
       bring_into_view
         ~at:scroll
         ~size:body_height
         ~start:card.y
         ~length:card.height)
;;

(* The same thing sideways, and it is not optional: a tree wider than the
   pane keeps its right-hand cards past the edge forever, so without this the
   cursor walks onto cards nobody can see and [wasd] looks broken. Nothing
   pans by hand — there is no horizontal wheel — so the offset is whatever it
   takes to show the card being pointed at, and zero when none is.

   It follows the selection once the cursor is committed, so [Enter] on a
   far-right card does not snap the pane back to the left — but only while
   the card is among the rows on screen. The scroll does not chase the
   selection the way it chases the cursor, so the selected card can sit far
   below the window; panning to its column anyway crops the rows you ARE
   looking at down to whatever happens to cross it, which on a big dump is
   nothing at all. *)
let follow_left
  placed
  ~body_width
  ~body_height
  ~scroll
  ~(selection : Selection.t)
  =
  match Option.first_some selection.cursor selection.selected with
  | None -> 0
  | Some { Spot.site; address = (_ : Snapshot.Address.t) } ->
    (match card_at placed site with
     | None -> 0
     | Some card ->
       (match
          card.y < scroll + body_height && card.y + card.height > scroll
        with
        | false -> 0
        | true ->
          Int.max
            0
            (bring_into_view
               ~at:0
               ~size:body_width
               ~start:card.x
               ~length:card.width)))
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

(* runs on the resolved scroll, so "among the rows on screen" means the rows
   the eye is actually getting *)
let resolve_left placed ~width ~height ~scroll ~selection =
  follow_left
    placed
    ~body_width:(Panel.inner_width ~width)
    ~body_height:(height - Panel.header_height)
    ~scroll
    ~selection
;;

let view
  ~note
  ~total
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
  let left = resolve_left placed ~width ~height ~scroll ~selection in
  let fresh = Set.length new_addresses in
  let live = List.length structures in
  let nodes = count_nodes structures in
  let meta =
    (* under a [/] filter the live count owns up to what it is hiding *)
    let living =
      match total with
      | Some total when total <> live ->
        [%string "%{live#Int} of %{total#Int} live"]
      | Some (_ : int) | None -> [%string "%{live#Int} live"]
    in
    let base = [%string "%{living} · %{nodes#Int} nodes"] in
    let base =
      match fresh with
      | 0 -> base
      | fresh -> [%string "%{base} · %{fresh#Int} new"]
    in
    match note with
    | None -> base
    | Some note -> [%string "%{note} · %{base}"]
  in
  Panel.view
    ~title:"heap"
    ~meta
    ~width
    ~height
    (View.crop ~t:scroll ~l:left canvas)
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
  let left = resolve_left placed ~width ~height ~scroll ~selection in
  let x = x + left in
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
  let left = resolve_left placed ~width ~height ~scroll ~selection in
  List.find placed ~f:(Placed.contains ~x:(x + left) ~y:(y + scroll))
  |> Option.map ~f:Placed.spot
;;

(* The structure a card belongs to, as the key that folds it. A card inside a
   referenced structure answers with THAT structure — the tree it is drawn in
   is the one collapsing round it, which is the one you were looking at. *)
let fold_of_spot ({ Spot.site; address = (_ : Snapshot.Address.t) } : Spot.t)
  =
  Fold.Structure site.structure
;;

(* Accordion mode's fold set: every structure closed but the one the keyboard
   is in. Recomputed from the selection on every render, which is what makes
   walking [w]/[s] across the registry open each structure on arrival and
   close it behind you. The manual set passes through underneath — node folds
   inside the open structure keep working — but structure folds are
   overridden wholesale while the mode is on: the others forced shut, the
   open one's cleared so arriving somewhere always opens it. *)
let accordion_folds ~structures ~folds ~(selection : Selection.t) =
  let standing_in =
    Option.first_some selection.cursor selection.selected
    |> Option.map
         ~f:(fun { Spot.site; address = (_ : Snapshot.Address.t) } ->
           site.structure)
  in
  List.fold
    structures
    ~init:folds
    ~f:(fun folds (structure : Replay.Structure.t) ->
      match standing_in with
      | Some id when id = structure.id ->
        Set.remove folds (Fold.Structure structure.id)
      | Some (_ : int) | None -> Set.add folds (Fold.Structure structure.id))
;;

(* where the pane starts you off: a structure's own root card *)
let spot_of_structure (structure : Replay.Structure.t) =
  { Spot.address = structure.address
  ; site = { Site.structure = structure.id; path = []; is_header = false }
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
  (* where the last press left us — or, when that card is gone because its
     structure has since been collapsed, whatever still draws that node,
     which is the header standing in for the whole tree *)
  let from =
    Option.first_some selection.cursor selection.selected
    |> Option.bind ~f:(fun (spot : Spot.t) ->
      match find spot.site with
      | Some card -> Some card
      | None ->
        List.find placed ~f:(fun (card : Placed.t) ->
          Snapshot.Address.equal card.address spot.address))
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
