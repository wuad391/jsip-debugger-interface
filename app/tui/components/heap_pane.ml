open! Core
open Jsip_types
open Jsip_replay
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

(* Which drawing of a node the keyboard is standing on. A node can be on the
   canvas twice — its own row, and a [↗] row pointing at it from a structure
   that shares it — and those are two places even though they are one object,
   so a position has to say which one it means. Keyed like a fold: the owning
   structure, and the edge path down to the row. *)
module Site = struct
  type t =
    { structure : int
    ; path : int list
    ; is_header : bool
    (** the structure's own row rather than a row inside it. A collapsed
        structure is nothing but that row, so without somewhere to stand on
        it there would be no way back — you could fold a structure away and
        never reach it again. *)
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

(* which row is chosen and which one the keyboard is aiming at; either may be
   absent, and while you aim they are both on screen — blue behind, orange
   ahead. *)
module Selection = struct
  type t =
    { selected : Spot.t option
    ; cursor : Spot.t option
    }
  [@@deriving sexp_of, equal]

  let none = { selected = None; cursor = None }

  (* How a row is picked out. The one the keyboard is standing on wears the
     full treatment — wash, address, bright name. Any other drawing of the
     same node wears a muted wash of the same hue and the accent on its
     value: a sharing pointer is the one thing on the pane you are looking
     for from across it, so it has to be findable at a glance, while still
     losing to the row you are actually standing on. *)
  module Mark = struct
    type t =
      | Plain
      | Linked_to_selected
      | Linked_to_cursor
      | Selected
      | Cursor

    (* The row you chose or the one you are aiming at, as opposed to one
       merely linked to either. Only these two spell their address out. *)
    let is_picked = function
      | Selected | Cursor -> true
      | Linked_to_selected | Linked_to_cursor | Plain -> false
    ;;

    (* The wash under a picked row — the same one every pane uses for the
       line it is pointing at, so the outline cannot disagree with the stack
       about what "selected" looks like — and its muted half under the other
       drawing of that same node. *)
    let wash t =
      match t with
      | Cursor -> Some Theme.cursor_bg
      | Selected -> Some Theme.highlight_bg
      | Linked_to_cursor -> Some Theme.cursor_echo
      | Linked_to_selected -> Some Theme.highlight_echo
      | Plain -> None
    ;;

    (* A picked row and anything linked to it share an accent — that is how
       you find the other drawing of one node. [plain] is what a row wears
       when it is neither. *)
    let accent t ~plain =
      match t with
      | Cursor | Linked_to_cursor -> Theme.cursor
      | Selected | Linked_to_selected -> Theme.highlight
      | Plain -> plain
    ;;

    (* A row's name reads as the row's own, not as chrome — bright and bold
       where it is picked out, [plain] otherwise. *)
    let name_attrs t ~plain =
      match t with
      | Cursor -> [ Theme.fg Theme.cursor_deep; Attr.bold ]
      | Selected -> [ Theme.fg Theme.highlight_deep; Attr.bold ]
      | Linked_to_selected | Linked_to_cursor | Plain -> plain
    ;;
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

(* What a click can fold: a whole structure behind its own row, or any row's
   children behind it. Node paths are edge positions from the owning
   structure's root, so a fold survives re-walks and re-parenting. *)
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

(* everything reference-following needs while a step's outline is built: the
   registry, and which structures are already on it. Only [Id] blocks
   reference tracked structures — an [Address] block is a pointer the walker
   chose not to decode. *)
module Context = struct
  type t =
    { by_id : Replay.Structure.t Int.Map.t
    ; nodes : Replay.Nodes.t
    ; drawn : Int.Hash_set.t (** structures already placed in the outline *)
    ; drawn_nodes : Int.Hash_set.t
    (** node ids already drawn: the wire shares blocks between structures and
        versions (and a payload may even cycle), so a second occurrence
        points at the first instead of listing it again *)
    ; new_addresses : Snapshot.Address.Set.t
    }

  let create ~structures ~nodes ~new_addresses =
    { by_id =
        Int.Map.of_alist_reduce
          (List.map structures ~f:(fun (structure : Replay.Structure.t) ->
             structure.id, structure))
          ~f:(fun first (_ : Replay.Structure.t) -> first)
    ; nodes
    ; drawn = Int.Hash_set.create ()
    ; drawn_nodes = Int.Hash_set.create ()
    ; new_addresses
    }
  ;;

  (* an [Id] names a node the dump defined earlier — sometimes a tracked
     structure's root, sometimes a shared payload block *)
  let id_of (block : Snapshot.Block.t) =
    match block with
    | Id id -> Some id
    | Address _ | Int _ | Float _ | String _ | Int32 _ | Int64 _
    | Nativeint _ | Float_array _ | Child ->
      None
  ;;

  let node t block = Option.bind (id_of block) ~f:(Replay.Nodes.find t.nodes)
  let structure t block = Option.bind (id_of block) ~f:(Map.find t.by_id)
end

(* one node's outgoing edges and inline fields, straight off the wire *)
module Edge = struct
  (* a node already drawn elsewhere in this outline: the wire shares it, so
     the outline points at it rather than listing it twice. The pointer
     carries the node the id defines, so it can name what it points at the
     way that row does. *)
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

let is_positional label = String.for_all label ~f:Char.is_digit

(* A row is a line, so everything printed on one has to be. The compiler
   prints a long inferred type across several lines and a walked string can
   hold anything at all, and either would otherwise break the row apart and
   take the columns of every row after it with it. *)
let one_line text =
  let buffer = Buffer.create (String.length text) in
  String.iter text ~f:(fun char ->
    let char =
      match Char.is_whitespace char with true -> ' ' | false -> char
    in
    let after_space =
      Buffer.length buffer > 0
      && Char.equal (Buffer.nth buffer (Buffer.length buffer - 1)) ' '
    in
    match Char.equal char ' ' && after_space with
    | true -> ()
    | false -> Buffer.add_char buffer char);
  Buffer.contents buffer
;;

(* Read a node the way the wire writes it: [block] holds every kept field
   under its own label and in field order, a field holding a walked block
   reads [Child] and stands for the next node in [children], and an [Id]
   names a node defined earlier. Nothing here needs a layout — the only
   per-structure knowledge left is {!Snapshot.Ds_type.interior_labels}, which
   says which [Int 0] is an empty pointer rather than the number.

   Returns the labeled edges in field order, and the leaf fields the row
   should print. *)
let node_edges (node : Snapshot.Node.t) ~ds_type ~(context : Context.t) =
  let children = Queue.of_list node.children in
  let interior = Snapshot.Ds_type.interior_labels ds_type in
  let is_interior label = List.mem interior label ~equal:String.equal in
  (* An [Id] names a node the dump defined earlier. If it is a tracked
     structure's root, that whole structure nests in here; otherwise it is a
     shared block, which lists here the first time and points back after. *)
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
     listing them beats dropping them *)
  let unclaimed =
    List.map (Queue.to_list children) ~f:(fun child -> "", Edge.Child child)
  in
  List.rev edges @ unclaimed, List.rev leaves
;;

(* the classic two-field payloads, by the labels the walker gives them: a map
   node's key and data, a hashtable entry's, a hash queue pair's *)
let binding_pairs = [ "v", "d"; "k", "v"; "key", "data" ]

let is_binding pair =
  List.mem binding_pairs pair ~equal:[%equal: string * string]
;;

(* The label a container puts its own size under. A root holding nothing but
   one of these is a record OVER its contents rather than one of them — the
   distinction {!root_is_entry} turns on. *)
let counter_labels = [ "length"; "size"; "len"; "num_readers" ]
let is_counter label = List.mem counter_labels label ~equal:String.equal

(* A positional field holding [Int 0] is dropped: an array's empty slots are
   [Int 0], and a bucket array is mostly empty slots, so printing them buries
   the one that is set under fifteen that are not. That does hide a literal
   zero sitting in a tuple — the trade the pane has always made, stated where
   it happens. *)
let printable_leaves leaves =
  List.filter leaves ~f:(fun (label, block) ->
    match is_positional label, (block : Snapshot.Block.t) with
    | true, Int 0 -> false
    | (true | false), _ -> true)
;;

(* the pieces a row's value is made of, so the colors are decided once *)
module Span = struct
  type kind =
    | Key
    | Value
    | Arrow
    | Label
    | Null (** there is nothing here — a word, not a mark, so it reads *)
    | Gap

  type t = kind * string

  let attrs kind ~accent =
    match kind with
    | Key -> [ Theme.fg Theme.text; Attr.bold ]
    | Value -> Theme.fg' accent
    | Arrow -> Theme.fg' Theme.ghost
    | Label -> Theme.fg' Theme.muted
    (* italic, because it is the pane talking and not the program: the one
       word on a row that was never in the heap *)
    | Null -> [ Theme.fg Theme.ghost; Attr.italic ]
    | Gap -> []
  ;;

  (* Attributed pieces, ready to wrap. [Arrow], [Null] and [Gap] are the
     pane's own punctuation and mean their spacing; everything else came off
     the wire, and gets flattened onto one line. *)
  let pieces ~accent spans =
    List.map spans ~f:(fun (kind, text) ->
      let text =
        match kind with
        | Key | Value | Label -> one_line text
        | Arrow | Null | Gap -> text
      in
      attrs kind ~accent, text)
  ;;

  (* the same pieces as one drawn line, for the diagram, whose boxes are laid
     out from their own measured widths rather than wrapped to a pane *)
  let view ~accent spans =
    View.hcat
      (List.map (pieces ~accent spans) ~f:(fun (attrs, text) ->
         View.text ~attrs text))
  ;;
end

(* What a row says where the wire gave it nothing to say: an unresolved
   revisit stub, a container whose entries are all empty slots. Spelled out
   rather than dotted — a reader has to be able to tell "nothing is here"
   from a bullet in a list. *)
let null_span = Span.Null, "null"

(* What a node says about itself. [key → data] where it holds one of the
   known binding pairs, [length n] for a counter, the bare value where there
   is only one, positional values joined where the labels are an array's or a
   tuple's, and [label=value] otherwise — user records included, which is why
   the arrow is not simply "any two fields".

   Returns LINES: a record with several fields gets one per line, which is
   how the diagram's boxes want them and how a record reads everywhere else
   OCaml prints one. Everything with one thing to say says it on one line. *)
let field_lines leaves ~arity =
  let kept = printable_leaves leaves in
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
  | [] when positional ->
    [ [ Span.Label, "slots "; Value, Int.to_string arity ] ]
  | [] -> [ [ null_span ] ]
  | fields when positional ->
    [ [ Span.Value, String.concat (List.map fields ~f:snd) ~sep:", " ] ]
  | [ (key_label, key); (data_label, data) ]
    when is_binding (key_label, data_label) ->
    [ [ Span.Key, key; Arrow, " → "; Value, data ] ]
  | [ (label, value) ] when is_counter label ->
    [ [ Span.Label, [%string "%{label} "]; Value, value ] ]
  | [ ((_ : string), value) ] -> [ [ Span.Value, value ] ]
  | fields ->
    List.map fields ~f:(fun (label, value) ->
      [ Span.Label, [%string "%{label}="]; Value, value ])
;;

(* The same thing as one line, for a row of the outline: a row IS a line, so
   a record reads [a=1  b=2] across it rather than down. *)
let summary_spans leaves ~arity =
  field_lines leaves ~arity
  |> List.intersperse ~sep:[ Span.Gap, "  " ]
  |> List.concat
;;

(* Whether the row's payload is a key-and-data pair — what makes a container
   count its rows as bindings rather than as elements. *)
let is_binding_payload leaves =
  match printable_leaves leaves with
  | [ ((key_label : string), (_ : Snapshot.Block.t))
    ; ((data_label : string), (_ : Snapshot.Block.t))
    ] ->
    is_binding (key_label, data_label)
  | (_ : (string * Snapshot.Block.t) list) -> false
;;

(* What the row over there is showing, so a pointer at it names the same
   thing it does.

   Deliberately NOT {!node_edges} — that claims references as it goes, and
   this is a second look at a node someone else is drawing. Claiming here
   would mark a structure drawn on behalf of a row that never draws it, and
   the structure would vanish from the pane. *)
let shared_leaves (node : Snapshot.Node.t) ~ds_type =
  let interior = Snapshot.Ds_type.interior_labels ds_type in
  List.filter node.block ~f:(fun (label, block) ->
    match (block : Snapshot.Block.t) with
    | Child | Id (_ : int) -> false
    | Int 0 -> not (List.mem interior label ~equal:String.equal)
    | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
    | Float_array _ | Address _ ->
      true)
;;

let shared_spans (node : Snapshot.Node.t) ~ds_type =
  summary_spans (shared_leaves node ~ds_type) ~arity:(List.length node.block)
;;

(* the labels a node carries, which is as much of its shape as the wire
   states — enough to recognize the next link of a chain *)
let field_labels (node : Snapshot.Node.t) = List.map node.block ~f:fst

(* The same shape, field for field: a map node's subtree is another map node,
   a cons cell's tail is another cons cell. An empty block — a revisit stub —
   would match everything, so it matches nothing. *)
let is_self_similar (node : Snapshot.Node.t) (target : Snapshot.Node.t) =
  (not (List.is_empty node.block))
  && List.equal String.equal (field_labels node) (field_labels target)
;;

(* Whether a structure's root node is one of the container's own entries
   rather than a record over them — the one thing that decides whether the
   structure's row counts its contents or summarizes them.

   A stdlib map's root IS an AVL node and a cons cell IS a list element: what
   they hold belongs to the first row underneath, and the structure's row
   says [2 bindings]. A queue's [{length; first; last}] and a Core map's
   [{length; tree}] are records over their contents: [length 2] is exactly
   what the structure's row has to say, and repeating it as a child would be
   a row that means nothing.

   What tells the two apart is what the root holds of its own: a size counter
   and nothing else belongs to a record, anything else to an entry.

   Read off the raw node rather than {!node_edges}, which claims references
   as it goes and must run once. *)
let root_is_entry (node : Snapshot.Node.t) ~ds_type =
  let interior = Snapshot.Ds_type.interior_labels ds_type in
  let is_interior label = List.mem interior label ~equal:String.equal in
  let payload =
    List.filter node.block ~f:(fun (label, block) ->
      match (block : Snapshot.Block.t) with
      | Child -> false
      | Int 0 -> not (is_interior label || is_positional label)
      | Id _ | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
      | Float_array _ | Address _ ->
        true)
  in
  (* it threads on through itself: either a walked child of its own shape, or
     an empty skeleton slot where one would have been *)
  let threads_itself =
    List.exists node.children ~f:(is_self_similar node)
    || List.exists node.block ~f:(fun (label, block) ->
      match (block : Snapshot.Block.t) with
      | Int 0 -> is_interior label
      | Child | Id _ | Int _ | Float _ | String _ | Int32 _ | Int64 _
      | Nativeint _ | Float_array _ | Address _ ->
        false)
  in
  (* a size and nothing else is a record's payload: no container keeps its
     own length inside one of its entries. This is what stops an EMPTY Core
     map from reading as an entry — its empty [tree] slot otherwise looks
     exactly like a lone AVL node's empty subtree. *)
  let counter_only =
    match payload with
    | [ (label, (_ : Snapshot.Block.t)) ] -> is_counter label
    | (_ : (string * Snapshot.Block.t) list) -> false
  in
  (not (List.is_empty payload)) && (not counter_only) && threads_itself
;;

(* One entry of the outline before it is flattened into lines: what reached
   it, what it is, what it holds, and what hangs under it. *)
module Entry = struct
  type t =
    { field : string (** the edge label that reached it, or [""] *)
    ; name : string (** a structure's name; [""] on a plain entry *)
    ; ty : string (** a structure's type; [""] on a plain entry *)
    ; value : Span.t list
    ; address : Snapshot.Address.t
    ; is_new : bool
    ; is_current : bool (** the structure this step's event walked *)
    ; is_binding : bool
    ; is_pointer : bool
    ; site : Site.t
    ; fold : Fold.t
    ; folded : bool
    ; children : t list (** built even when folded, so counts are known *)
    }

  let rec size t =
    List.sum (module Int) t.children ~f:(fun child -> 1 + size child)
  ;;
end

(* how a container's row counts what is under it: bindings where its entries
   are pairs, elements otherwise *)
let count_label (entries : Entry.t list) =
  let word =
    match List.exists entries ~f:(fun entry -> entry.is_binding) with
    | true -> "binding"
    | false -> "element"
  in
  match List.length entries with
  | 1 -> [%string "1 %{word}"]
  | count -> [%string "%{count#Int} %{word}s"]
;;

(* The name a structure goes by, and the type to print beside it. The wire's
   own printed type is what a reader recognizes — [int M.t], [int list] — so
   it wins where the event stated one; the walked kind stands in where it did
   not. *)
let structure_name (structure : Replay.Structure.t) =
  Replay.Structure.display structure
;;

let structure_type (structure : Replay.Structure.t) =
  match structure.ty with
  (* the compiler prints a long inferred type across several lines, and a row
     is one line *)
  | Some ty -> one_line ty.Type_info.printed
  | None -> Snapshot.Ds_type.display structure.snapshot.ds_type
;;

(* One node's rows, at the level it was reached on. Three rules turn a walked
   heap shape into an outline, and none of them needs to know which container
   it is looking at:

   - a node with nothing of its own to print is plumbing — a hashtable's
     bucket array, a wrapper record — so it gets no row and the things it
     points at take its place, here;
   - an edge whose label is the structure's own skeleton
     ({!Snapshot.Ds_type.interior_labels}) carries on through the container
     rather than descending into it, so what it reaches lists as a SIBLING;
   - so does an edge landing on a node shaped exactly like this one — the
     tail of a cons cell, the subtree of a map node — which is what flattens
     a chain or a tree into a plain list of what it holds.

   Everything else is content, and nests. Together that turns an AVL tree
   into its bindings, a bucket array into its entries, and a cons chain into
   its elements, while a record's fields still hang underneath it. *)
let rec entries_of
  (node : Snapshot.Node.t)
  ~field
  ~ds_type
  ~(context : Context.t)
  ~folds
  ~structure_id
  ~path
  =
  Hash_set.add context.drawn_nodes node.id;
  let edges, leaves = node_edges node ~ds_type ~context in
  let interior = Snapshot.Ds_type.interior_labels ds_type in
  let is_interior label = List.mem interior label ~equal:String.equal in
  let continues ((label : string), (edge : Edge.t)) =
    match edge with
    (* an empty skeleton slot is part of the container and adds nothing to it
       either way — but it is not a child, or every binding in a tree would
       look like it had two *)
    | Nil -> true
    (* a subtree the outline already listed is still this container's
       subtree: where it sits is all that changed, so its pointer belongs
       beside the bindings it shares a level with, not under one of them *)
    | Child target | Shared { id = _; node = Some target } ->
      is_interior label || is_self_similar node target
    | Shared { id = _; node = None } -> is_interior label
    (* a tracked structure in a skeleton slot keeps its own subtree: that is
       what nests the map under the queue element it became *)
    | Ref (_ : Replay.Structure.t) -> false
  in
  (* Skeleton labels and array indices are plumbing, not names: a map binding
     reached through [l] is not called [l]. A field the program itself
     declared is, so it stays. *)
  let shown label =
    match is_interior label || is_positional label with
    | true -> ""
    | false -> label
  in
  let expand index ((label : string), (edge : Edge.t)) =
    let path = path @ [ index ] in
    match edge with
    | Nil -> []
    | Child target ->
      entries_of
        target
        ~field:(shown label)
        ~ds_type
        ~context
        ~folds
        ~structure_id
        ~path
    | Ref structure ->
      [ structure_entry structure ~field:(shown label) ~context ~folds ]
    | Shared { id; node = target } ->
      [ pointer_entry
          ~id
          ~target
          ~field:(shown label)
          ~ds_type
          ~structure_id
          ~path
      ]
  in
  (* expanded in field order, so which reference is claimed and which is
     pointed at does not depend on how the rows are later split up *)
  let expanded =
    List.mapi edges ~f:(fun index edge -> continues edge, expand index edge)
  in
  let of_kind kind =
    List.concat_map
      (List.filter expanded ~f:(fun (continuing, (_ : Entry.t list)) ->
         Bool.equal continuing kind))
      ~f:snd
  in
  let row ~value ~is_binding ~children =
    let fold = Fold.Node (structure_id, path) in
    { Entry.field
    ; name = ""
    ; ty = ""
    ; value
    ; address = node.virtual_address
    ; is_new = Set.mem context.new_addresses node.virtual_address
    ; is_current = false
    ; is_binding
    ; is_pointer = false
    ; site = { Site.structure = structure_id; path; is_header = false }
    ; fold
    ; folded = Set.mem folds fold
    ; children
    }
  in
  match printable_leaves leaves, edges with
  (* plumbing with somewhere to send its contents *)
  | [], _ :: _ -> of_kind false @ of_kind true
  (* nothing to say and nothing to splice into — a revisit stub the registry
     did not resolve. A row saying [null] beats a node quietly disappearing. *)
  | [], [] -> [ row ~value:[ null_span ] ~is_binding:false ~children:[] ]
  | printable, (_ : (string * Edge.t) list) ->
    let children = of_kind false in
    (* A binding whose data is a block of its own keeps the key here and the
       data on the line below. Left alone the key would read as a leaf, so it
       says [→] instead: this is half a binding, the rest is under it. *)
    let value = summary_spans printable ~arity:(List.length node.block) in
    let value =
      match
        printable, List.filter edges ~f:(fun edge -> not (continues edge))
      with
      | ( [ (key_label, (_ : Snapshot.Block.t)) ]
        , [ (data_label, (_ : Edge.t)) ] )
        when is_binding (key_label, data_label) ->
        value @ [ Span.Arrow, " →" ]
      | (_ : (string * Snapshot.Block.t) list), (_ : (string * Edge.t) list)
        ->
        value
    in
    row ~value ~is_binding:(is_binding_payload leaves) ~children
    :: of_kind true

(* A tracked structure's own row: its name, its type, and either what its
   root record says or how much it holds. Its contents hang underneath.

   Built even when folded, because the reference claims made while walking it
   have to hold either way — a folded structure keeps the structures it
   references tucked inside it rather than spilling them back out as rows of
   their own. *)
and structure_entry (structure : Replay.Structure.t) ~field ~context ~folds =
  let ds_type = structure.snapshot.ds_type in
  let root = Replay.Structure.current_root structure in
  let fold = Fold.Structure structure.id in
  let rows =
    entries_of
      root
      ~field:""
      ~ds_type
      ~context
      ~folds
      ~structure_id:structure.id
      ~path:[]
  in
  let value, children =
    match rows with
    | first :: rest
      when (not (root_is_entry root ~ds_type))
           && (not first.is_pointer)
           && Snapshot.Address.equal first.address root.virtual_address ->
      (* the root was a record over the contents: its own summary is the
         structure's, and its children are the structure's *)
      first.value, first.children @ rest
    | (_ : Entry.t list) -> [ Span.Label, count_label rows ], rows
  in
  { Entry.field
  ; name = structure_name structure
  ; ty = structure_type structure
  ; value
  ; address = structure.address
  ; is_new = Set.mem context.new_addresses structure.address
  ; is_current = structure.is_current
  ; is_binding = false
  ; is_pointer = false
  ; site = { Site.structure = structure.id; path = []; is_header = true }
  ; fold
  ; folded = Set.mem folds fold
  ; children
  }

(* A node the outline has already listed, pointed at rather than listed twice
   — which is the whole point of a persistent structure: two versions of a
   map share their subtrees, and repeating them would bury the one binding
   that differs.

   Named by what its target holds rather than by the wire's node number:
   [↗ "b" → 2] names something on screen, [↗ #11] names nothing. It answers
   to the target's address, so aiming at either the pointer or the row lights
   up both. *)
and pointer_entry ~id ~target ~field ~ds_type ~structure_id ~path =
  let value =
    match target with
    | Some node -> (Span.Arrow, "↗ ") :: shared_spans node ~ds_type
    | None -> [ Span.Arrow, "↗ "; Label, [%string "#%{id#Int}"] ]
  in
  { Entry.field
  ; name = ""
  ; ty = ""
  ; value
  ; address =
      (match target with
       | Some (node : Snapshot.Node.t) -> node.virtual_address
       | None -> 0n)
  ; is_new = false
  ; is_current = false
  ; is_binding = false
  ; is_pointer = true
  ; site = { Site.structure = structure_id; path; is_header = false }
  ; fold = Fold.Node (structure_id, path)
  ; folded = false
  ; children = []
  }
;;

(* Every live structure, in registry order. A structure referenced from
   another one nests inside its referrer instead of getting a row of its own
   — the registry still decides what is alive, only the placement moves. *)
let structure_entries ~structures ~nodes ~new_addresses ~folds =
  let context = Context.create ~structures ~nodes ~new_addresses in
  let referenced =
    List.fold structures ~init:Int.Set.empty ~f:(fun acc structure ->
      let owner = structure.Replay.Structure.id in
      Snapshot.Node.fold
        structure.Replay.Structure.snapshot.root_node
        ~init:acc
        ~f:(fun acc (node : Snapshot.Node.t) ->
          List.fold node.block ~init:acc ~f:(fun acc ((_ : string), block) ->
            match Context.structure context block with
            | Some (target : Replay.Structure.t) when target.id <> owner ->
              Set.add acc target.id
            | Some _ | None -> acc)))
  in
  let entry entries (structure : Replay.Structure.t) =
    match Hash_set.mem context.drawn structure.id with
    | true -> entries
    | false ->
      Hash_set.add context.drawn structure.id;
      structure_entry structure ~field:"" ~context ~folds :: entries
  in
  let top_level =
    List.filter structures ~f:(fun (structure : Replay.Structure.t) ->
      not (Set.mem referenced structure.id))
  in
  let acc = List.fold top_level ~init:[] ~f:entry in
  (* mutually-referencing structures have no unreferenced root; anything
     still unlisted gets a row of its own after all *)
  List.rev (List.fold structures ~init:acc ~f:entry)
;;

(* One row of the outline: an entry plus where it sits. [guide] is the
   assembled [├─ ]/[└─ ] run, ancestors' bars and all, so drawing and
   hit-testing read the same string. A row is not a line — a wide one wraps —
   so [continuation] is the guide with this row's own elbow turned back into
   a bar, which is both what a wrapped line hangs under and what this row's
   children hang under. *)
module Row = struct
  type t =
    { guide : string
    ; continuation : string
    ; depth : int
    ; parent : Site.t option
    ; entry : Entry.t
    ; hidden : int (** rows tucked away under a folded one *)
    }

  let spot t = { Spot.address = t.entry.address; site = t.entry.site }
  let site t = t.entry.site

  (* the column the fold glyph draws in — one cell, and the only part of a
     row that folds rather than selects *)
  let glyph_column t = View.width (View.text t.guide)
  let can_fold t = t.hidden > 0
end

(* The outline, one row per entry the folds leave visible. Guides are built
   on the way down: a run of [│  ]/[   ] for the ancestors, then this row's
   own elbow. A structure's row is the outline's top level and carries no
   guide.

   A row's [continuation] and its children's [prefix] are the same string,
   and for the same reason — both are what still hangs off this row once its
   elbow has been drawn. *)
let rows ~structures ~nodes ~new_addresses ~folds =
  let rec walk (entries : Entry.t list) ~prefix ~depth ~parent acc =
    let last = List.length entries - 1 in
    List.foldi entries ~init:acc ~f:(fun index acc (entry : Entry.t) ->
      let is_last = index = last in
      let guide, continuation =
        match depth with
        | 0 -> "", ""
        | _ ->
          ( (prefix ^ match is_last with true -> "└─ " | false -> "├─ ")
          , prefix ^ (match is_last with true -> "   " | false -> "│  ") )
      in
      let hidden = Entry.size entry in
      let acc =
        { Row.guide; continuation; depth; parent; entry; hidden } :: acc
      in
      match entry.folded with
      | true -> acc
      | false ->
        walk
          entry.children
          ~prefix:continuation
          ~depth:(depth + 1)
          ~parent:(Some entry.site)
          acc)
  in
  let entries = structure_entries ~structures ~nodes ~new_addresses ~folds in
  List.rev (walk entries ~prefix:"" ~depth:0 ~parent:None [])
;;

let count_nodes structures =
  List.sum
    (module Int)
    structures
    ~f:(fun (structure : Replay.Structure.t) ->
      Snapshot.Node.fold
        structure.snapshot.root_node
        ~init:0
        ~f:(fun n (_ : Snapshot.Node.t) -> n + 1))
;;

let node_count_label count =
  match count with 1 -> "1 node" | count -> [%string "%{count#Int} nodes"]
;;

(* the name · kind · type line — everything a structure says about itself,
   and so also what [/] lets you filter by *)
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

let glyph_of ~folded = match folded with true -> "▸" | false -> "▾"

(* A row as it ends up on the canvas: the lines it occupies and the wash
   under them. A row is not a line — a wide one wraps rather than running off
   the edge — so everything that counts lines (scrolling, hit-testing) counts
   them here rather than counting rows. *)
module Drawn = struct
  type t =
    { row : Row.t
    ; lines : View.t list
    ; bg : Attr.Color.t option
    }

  let height t = List.length t.lines
  let total_lines drawn = List.sum (module Int) drawn ~f:height

  (* the row a canvas line belongs to, and how far into that row it is: line
     0 is the one carrying the glyph, the rest are its wrapped tail *)
  let at_line drawn ~line =
    let rec go drawn line =
      match drawn with
      | [] -> None
      | (t : t) :: rest ->
        (match line < height t with
         | true -> Some (t, line)
         | false -> go rest (line - height t))
    in
    match line < 0 with true -> None | false -> go drawn line
  ;;

  (* the canvas line a row starts on *)
  let line_of drawn ~index = total_lines (List.take drawn index)
end

(* One row of the outline, drawn: the guides, the fold glyph, the field that
   reached this row, the structure's name and type where it has them, what it
   holds, a [⋯ n] where it is hiding rows, a green [new] where this step
   allocated it — and, at the right margin, on the row the keyboard is
   standing on and only there, its address.

   Nothing is cropped. A row too wide for the pane wraps onto continuation
   lines, indented past its guide and under its own first column, so the
   outline's shape survives the break the way the stack pane's does.

   The address is placed after the wrapping is settled — on the last line if
   that line has room, on one of its own if it does not — so a row never
   reflows around its own address. The reverse, reserving the margin on every
   row whether or not it is being read, would cost a sixth of a narrow pane
   to a string that is usually not there. *)
let row_lines (row : Row.t) ~width ~selection =
  let entry = row.entry in
  let mark =
    Selection.mark selection ~address:entry.address ~site:entry.site
  in
  let accent = Selection.Mark.accent mark ~plain:Theme.ident in
  let piece ~attrs text =
    match String.is_empty text with true -> [] | false -> [ attrs, text ]
  in
  let name_attrs =
    Selection.Mark.name_attrs
      mark
      ~plain:
        (match entry.is_current with
         | true -> [ Theme.fg Theme.highlight_deep; Attr.bold ]
         | false -> [ Theme.fg Theme.text; Attr.bold ])
  in
  let hidden =
    match entry.folded && row.hidden > 0 with
    | false -> []
    | true -> [ Theme.fg' Theme.faint, [%string "⋯ %{row.hidden#Int}"] ]
  in
  let tag =
    match entry.is_new with
    | false -> []
    | true -> [ Theme.fg' Theme.fresh, "new" ]
  in
  (* two cells between the columns that are there, and none trailing the last
     one *)
  let columns =
    List.filter
      [ piece ~attrs:(Theme.fg' Theme.app_purple) entry.field
      ; piece ~attrs:name_attrs entry.name
      ; piece ~attrs:(Theme.fg' Theme.type_name) entry.ty
      ; Span.pieces ~accent entry.value
      ; hidden
      ; tag
      ]
      ~f:(fun column -> not (List.is_empty column))
  in
  let body = List.concat (List.intersperse columns ~sep:[ [], "  " ]) in
  (* the glyph column is two cells whether or not this row has a glyph, and a
     wrapped line hangs two further in, under the row's own first column *)
  let indent = Row.glyph_column row in
  let wrapped =
    Wrap.spans
      body
      ~first_width:(Int.max 8 (width - indent - 2))
      ~width:(Int.max 8 (width - indent - 4))
  in
  let glyph =
    match Row.can_fold row with
    | false -> View.text "  "
    | true ->
      View.hcat
        [ View.text
            ~attrs:(Theme.fg' Theme.faint)
            (glyph_of ~folded:entry.folded)
        ; View.text " "
        ]
  in
  let lead index =
    match index with
    | 0 -> [ View.text ~attrs:(Theme.fg' Theme.border) row.guide; glyph ]
    | _ ->
      [ View.text ~attrs:(Theme.fg' Theme.border) row.continuation
      ; View.text "    "
      ]
  in
  let lines =
    List.mapi wrapped ~f:(fun index pieces ->
      View.hcat
        (lead index
         @ List.map pieces ~f:(fun (attrs, text) -> View.text ~attrs text)))
  in
  let lines =
    match Selection.Mark.is_picked mark with
    | false -> lines
    | true ->
      let chip =
        View.text
          ~attrs:(Theme.fg' Theme.secondary)
          (Snapshot.Address.display entry.address)
      in
      let right_align line =
        let gap = Int.max 0 (width - View.width line - View.width chip) in
        View.hcat
          [ line; View.transparent_rectangle ~width:gap ~height:1; chip ]
      in
      let fits line = View.width line + View.width chip + 2 <= width in
      (match List.rev lines with
       | [] -> lines
       | last :: earlier ->
         let leading = List.rev earlier in
         (match fits last with
          | true -> leading @ [ right_align last ]
          | false -> leading @ [ last; right_align (View.text "") ]))
  in
  { Drawn.row; lines; bg = Selection.Mark.wash mark }
;;

(* The whole outline, drawn and washed. Every line is padded to the pane's
   width, so a picked row's wash runs the full width of each of its lines. *)
let canvas drawn ~width =
  View.vcat
    (List.concat_map drawn ~f:(fun (t : Drawn.t) ->
       List.map t.lines ~f:(fun line -> Panel.row ?bg:t.bg line ~width)))
;;

let bring_into_view ~at ~start ~length =
  match at < start, at >= start + length with
  | true, _ -> at
  | false, true -> at - length + 1
  | false, false -> start
;;

let clamp value ~max = Int.max 0 (Int.min max value)
let body_height ~height = Int.max 1 (height - Panel.header_height)

(* where the row the keyboard is on sits in the outline — by site, or by
   address when that site is gone because its structure has since collapsed *)
let row_index rows (spot : Spot.t) =
  match
    List.findi rows ~f:(fun (_ : int) (row : Row.t) ->
      Site.equal (Row.site row) spot.site)
  with
  | Some (index, (_ : Row.t)) -> Some index
  | None ->
    List.findi rows ~f:(fun (_ : int) (row : Row.t) ->
      Snapshot.Address.equal row.entry.address spot.address)
    |> Option.map ~f:fst
;;

let aimed_index rows ~(selection : Selection.t) =
  Option.first_some selection.cursor selection.selected
  |> Option.bind ~f:(row_index rows)
;;

(* every row wrapped and washed, ready to be counted in lines *)
let drawn rows ~width ~selection =
  List.map rows ~f:(row_lines ~width:(Panel.inner_width ~width) ~selection)
;;

(* The outline scrolls to keep the aimed row on screen, adjusting from
   wherever the reader had left it. [scroll] counts canvas lines rather than
   rows, because that is what a wrapped outline can be scrolled to; a row
   taller than the pane shows its head, which is where its name is. *)
let resolve_scroll drawn ~height ~scroll ~selection =
  let length = body_height ~height in
  let scroll = clamp scroll ~max:(Drawn.total_lines drawn - length) in
  match
    aimed_index (List.map drawn ~f:(fun (t : Drawn.t) -> t.row)) ~selection
  with
  | None -> scroll
  | Some index ->
    let first = Drawn.line_of drawn ~index in
    let last = first + Drawn.height (List.nth_exn drawn index) - 1 in
    let scroll = bring_into_view ~at:last ~start:scroll ~length in
    bring_into_view ~at:first ~start:scroll ~length
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
  let rows = rows ~structures ~nodes ~new_addresses ~folds in
  let drawn = drawn rows ~width ~selection in
  let scroll = resolve_scroll drawn ~height ~scroll ~selection in
  let fresh = Set.length new_addresses in
  let live = List.length structures in
  let node_count = count_nodes structures in
  let meta =
    (* under a [/] filter the live count owns up to what it is hiding *)
    let living =
      match total with
      | Some total when total <> live ->
        [%string "%{live#Int} of %{total#Int} live"]
      | Some (_ : int) | None -> [%string "%{live#Int} live"]
    in
    let base = [%string "%{living} · %{node_count_label node_count}"] in
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
    (View.crop ~t:scroll (canvas drawn ~width:(Panel.inner_width ~width)))
;;

(* the row a click at pane-body position [(x, y)] is on, and how far into
   that row's own lines it landed *)
let hit
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~scroll
  ~selection
  ~width
  ~height
  ~y
  =
  let rows = rows ~structures ~nodes ~new_addresses ~folds in
  let drawn = drawn rows ~width ~selection in
  let scroll = resolve_scroll drawn ~height ~scroll ~selection in
  Drawn.at_line drawn ~line:(y + scroll)
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
  match
    hit
      ~structures
      ~nodes
      ~new_addresses
      ~folds
      ~scroll
      ~selection
      ~width
      ~height
      ~y
  with
  (* the glyph is on the row's first line; a wrapped tail has no glyph to hit *)
  | Some (({ row; _ } : Drawn.t), 0) ->
    (match Row.can_fold row && x = Row.glyph_column row with
     | false -> None
     | true -> Some row.entry.fold)
  | Some ((_ : Drawn.t), (_ : int)) | None -> None
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
  ~x:(_ : int)
  ~y
  =
  hit
    ~structures
    ~nodes
    ~new_addresses
    ~folds
    ~scroll
    ~selection
    ~width
    ~height
    ~y
  |> Option.map ~f:(fun (({ row; _ } : Drawn.t), (_ : int)) -> Row.spot row)
;;

(* What [h] folds is what the cursor is on — which is exactly what the glyph
   beside it already says: on a structure's row, the whole structure; on a
   row inside one, that row's children. Node folds also survive accordion
   mode, where structure folds are the mode's to decide. *)
let fold_of_spot ({ Spot.site; address = (_ : Snapshot.Address.t) } : Spot.t)
  =
  match site.is_header with
  | true -> Fold.Structure site.structure
  | false -> Fold.Node (site.structure, site.path)
;;

(* Accordion mode's fold set: every structure closed but the one the keyboard
   is in. Recomputed from the selection on every render, which is what makes
   walking the registry open each structure on arrival and close it behind
   you. The manual set passes through underneath — node folds inside the open
   structure keep working — but structure folds are overridden wholesale
   while the mode is on: the others forced shut, the open one's cleared so
   arriving somewhere always opens it. *)
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

(* where the pane starts you off: a structure's own row *)
let spot_of_structure (structure : Replay.Structure.t) =
  { Spot.address = structure.address
  ; site = { Site.structure = structure.id; path = []; is_header = true }
  }
;;

(* A spot picked at one step can name a row another step does not draw:
   committing a [↗] pointer jumps the replay to where that node was
   allocated, and the structure the pointer lived in need not have existed
   back there. The node itself usually survives the jump, so re-point the
   spot at whatever row draws it now — [None] only when nothing in the
   outline is that node at all, and the pane falls back to the walked
   structure. *)
let resolve_spot
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ({ Spot.address; site } as spot)
  =
  let rows = rows ~structures ~nodes ~new_addresses ~folds in
  match
    List.exists rows ~f:(fun (row : Row.t) -> Site.equal (Row.site row) site)
  with
  | true -> Some spot
  | false ->
    (* the node's own row if it has one, since that is where it lives; a
       pointer at it only if nothing else in the outline draws it *)
    let drawings =
      List.filter rows ~f:(fun (row : Row.t) ->
        Snapshot.Address.equal row.entry.address address)
    in
    (match
       List.find drawings ~f:(fun (row : Row.t) -> not row.entry.is_pointer)
     with
     | Some row -> Some (Row.spot row)
     | None -> List.hd drawings |> Option.map ~f:Row.spot)
;;

(* The cursor walks the outline the way a file tree walks: [Up] and [Down]
   step to the line above and below, across structure boundaries and all, so
   one key runs the whole pane top to bottom. [Left] climbs to the row this
   one hangs under and [Right] drops into the first row under this one — the
   two moves a flat list of lines cannot express by itself.

   A folded row has no rows under it to drop into, which is the point: fold
   what you are done with and [Down] steps past it. *)
let move_cursor
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~(selection : Selection.t)
  ~(direction : Direction.t)
  =
  let rows = rows ~structures ~nodes ~new_addresses ~folds in
  match aimed_index rows ~selection with
  | None -> List.hd rows |> Option.map ~f:Row.spot
  | Some index ->
    let row = List.nth_exn rows index in
    let moved =
      match direction with
      | Up -> List.nth rows (index - 1)
      | Down -> List.nth rows (index + 1)
      | Left ->
        Option.bind row.parent ~f:(fun parent ->
          List.find rows ~f:(fun (other : Row.t) ->
            Site.equal (Row.site other) parent))
      | Right ->
        (match List.nth rows (index + 1) with
         | Some (child : Row.t) when child.depth > row.depth -> Some child
         | Some (_ : Row.t) | None -> None)
    in
    Option.map moved ~f:Row.spot
;;

(* which structure a position is inside — what the diagram pop-out is asked
   for, since a row is only ever read as part of one *)
let structure_of_spot
  ({ Spot.site; address = (_ : Snapshot.Address.t) } : Spot.t)
  =
  site.structure
;;

(* The heap's OTHER rendering: one structure as the diagram it physically is
   — a box per node, rails for the pointers between them, children spread
   under their parent — popped out over the outline and dismissed with
   [Escape].

   The outline is the everyday view precisely because it hides this: a map is
   an AVL tree, and reading five bindings should not mean reading five levels
   of rebalancing. But the tree is what the program actually built, and there
   are questions only its shape answers — why an [add] rebuilt three nodes,
   which subtree two versions share, how deep a bucket chain ran. That is
   what this is for, and why it is a pop-out rather than a mode: you arrive
   with a question and leave with an answer.

   {v
              ┌ m ────────┐
              │"d" → 4    │
              └───────────┘
               ┌─────┴─────┐
               l           r
   ┌───────────┐       ┌┄ ↗ ┄┄┄┄┄┄┐
   │"b" → 2    │       ┆ "j" → 10 ┆
   └───────────┘       └┄┄┄┄┄┄┄┄┄┄┘
   v}

   No selection and no folding in here — one structure at a time and nothing
   to aim at, so a box is only ever a box. Scroll and pan are the whole
   interaction, which is where the outline's freed [\[]/[\]] went. *)
module Diagram = struct
  (* the space between siblings; also what keeps two rails from touching *)
  let sibling_gap = 3

  (* a node's box: what it holds, a field to a line, with the structure's
     name riding the top-left border where this node is one's root and a
     green [new] tag riding the top right where this step allocated it *)
  let node_box
    (node : Snapshot.Node.t)
    ~leaves
    ~arity
    ~(context : Context.t)
    ~name
    =
    let border = Theme.fg' Theme.card_border in
    let lines =
      List.map (field_lines leaves ~arity) ~f:(Span.view ~accent:Theme.ident)
    in
    let name_tag =
      match name with
      | None -> View.none
      | Some name ->
        View.text
          ~attrs:[ Theme.fg Theme.text; Attr.bold ]
          [%string " %{name} "]
    in
    let new_tag =
      match Set.mem context.new_addresses node.virtual_address with
      | false -> View.none
      | true -> View.text ~attrs:(Theme.fg' Theme.fresh) " new "
    in
    let riders = View.width name_tag + View.width new_tag in
    let inner =
      List.fold lines ~init:riders ~f:(fun widest line ->
        Int.max widest (View.width line))
    in
    let rows =
      (View.hcat
         [ View.text ~attrs:border "┌"
         ; name_tag
         ; View.text ~attrs:border (Panel.repeat "─" ~width:(inner - riders))
         ; new_tag
         ; View.text ~attrs:border "┐"
         ]
       :: List.map lines ~f:(fun line ->
         View.hcat
           [ View.text ~attrs:border "│"
           ; Panel.fit line ~width:inner ~height:1
           ; View.text ~attrs:border "│"
           ]))
      @ [ View.text
            ~attrs:border
            [%string "└%{Panel.repeat \"─\" ~width:inner}┘"]
        ]
    in
    Panel.fit (View.vcat rows) ~width:(inner + 2) ~height:(List.length rows)
  ;;

  (* An empty slot is still a slot, so it gets a box too — dotted and grayed.
     A bare [∅] hanging off a rail read as an annotation on the edge; a box
     reads as what it is, the thing the pointer does not point at. *)
  let nil_box =
    let attrs = Theme.fg' Theme.ghost in
    View.vcat
      [ View.text ~attrs "┌┄┄┄┐"
      ; View.text ~attrs "┆ ∅ ┆"
      ; View.text ~attrs "└┄┄┄┘"
      ]
  ;;

  (* A node the diagram has already drawn, pointed at rather than drawn twice
     — which is the whole point of a persistent structure: two versions of a
     map share their subtrees, and redrawing them would bury the one node
     that differs. Dashed, to say it is not the original, and named by what
     its target holds rather than by the wire's node number: [↗ "b" → 2]
     names something on screen, [↗ #11] names nothing. *)
  let shared_box target ~id ~ds_type =
    let border = Theme.fg' Theme.ghost in
    let lines =
      match target with
      | Some (node : Snapshot.Node.t) ->
        List.map
          (field_lines
             (shared_leaves node ~ds_type)
             ~arity:(List.length node.block))
          ~f:(Span.view ~accent:Theme.ident)
      | None ->
        [ View.text ~attrs:(Theme.fg' Theme.muted) [%string "#%{id#Int}"] ]
    in
    let arrow = View.text ~attrs:(Theme.fg' Theme.muted) " ↗ " in
    let inner =
      List.fold
        lines
        ~init:(View.width arrow - 2)
        ~f:(fun widest line -> Int.max widest (View.width line))
    in
    let rows =
      (View.hcat
         [ View.text ~attrs:border "┌"
         ; arrow
         ; View.text
             ~attrs:border
             (Panel.repeat "┄" ~width:(inner + 2 - View.width arrow))
         ; View.text ~attrs:border "┐"
         ]
       :: List.map lines ~f:(fun line ->
         View.hcat
           [ View.text ~attrs:border "┆ "
           ; Panel.fit line ~width:inner ~height:1
           ; View.text ~attrs:border " ┆"
           ]))
      @ [ View.text
            ~attrs:border
            [%string "└%{Panel.repeat \"┄\" ~width:(inner + 2)}┘"]
        ]
    in
    Panel.fit (View.vcat rows) ~width:(inner + 4) ~height:(List.length rows)
  ;;

  (* the [┌──┴──┐] rail between a parent and its children, hooked at each
     child's center. These lines are the diagram's pointers, so they read a
     shade ahead of the boxes rather than behind them. *)
  let rail ~parent_center ~centers =
    let leftmost =
      List.min_elt centers ~compare:Int.compare |> Option.value ~default:0
    in
    let rightmost =
      List.max_elt centers ~compare:Int.compare |> Option.value ~default:0
    in
    let child_centers = Int.Set.of_list centers in
    let glyph x =
      let is_child = Set.mem child_centers x in
      match x < leftmost || x > rightmost with
      | true -> " "
      | false ->
        (match x = parent_center, is_child with
         (* a lone child hangs straight down; an aligned middle child crosses
            the rail *)
         | true, true ->
           (match leftmost = rightmost with true -> "│" | false -> "┼")
         | true, false -> "┴"
         | false, true ->
           (match x = leftmost, x = rightmost with
            | true, _ -> "┌"
            | _, true -> "┐"
            | false, false -> "┬")
         | false, false -> "─")
    in
    (* a Buffer rather than [String.concat (List.init ...)]: the glyphs are
       multi-byte, and a rail is as wide as the widest row of boxes *)
    let buffer = Buffer.create ((rightmost + 1) * 3) in
    for x = 0 to rightmost do
      Buffer.add_string buffer (glyph x)
    done;
    View.text ~attrs:(Theme.fg' Theme.rail) (Buffer.contents buffer)
  ;;

  (* edge labels sitting under their hooks *)
  let rail_labels ~labeled_centers =
    let width =
      List.fold labeled_centers ~init:0 ~f:(fun width (center, label) ->
        Int.max
          width
          (center + 1 + (String.length label / 2) + String.length label))
    in
    let buffer = Bytes.make width ' ' in
    List.iter labeled_centers ~f:(fun (center, label) ->
      let start = Int.max 0 (center - (String.length label / 2)) in
      String.iteri label ~f:(fun index char ->
        let at = start + index in
        match at < width with
        | true -> Bytes.set buffer at char
        | false -> ()));
    View.text
      ~attrs:(Theme.fg' Theme.muted)
      (Bytes.to_string buffer |> String.rstrip)
  ;;

  (* Lay the subtree out the way a CS diagram draws it: the node's box
     centered over its children, siblings side by side on one level, a rail
     from the box down to each child's center. A field holding an [Id] into
     the registry links that structure's whole tree in as a child — each is
     drawn once, so a second reference (or a cycle) stays a dashed pointer.

     Returns the canvas, the column the node's own box is centered on, and
     how wide the subtree came out. *)
  let rec tree (node : Snapshot.Node.t) ~ds_type ~(context : Context.t) ~name
    =
    let edges, leaves = node_edges node ~ds_type ~context in
    (* a leaf keeps its empty slots to itself *)
    let edges =
      match
        List.for_all edges ~f:(fun ((_ : string), (edge : Edge.t)) ->
          match edge with Nil -> true | Child _ | Ref _ | Shared _ -> false)
      with
      | true -> []
      | false -> edges
    in
    Hash_set.add context.drawn_nodes node.id;
    let box =
      node_box node ~leaves ~arity:(List.length node.block) ~context ~name
    in
    let box_width = View.width box in
    match edges with
    | [] -> box, box_width / 2, box_width
    | edges ->
      let rendered =
        List.map edges ~f:(fun (label, (edge : Edge.t)) ->
          match edge with
          | Nil ->
            label, (nil_box, View.width nil_box / 2, View.width nil_box)
          | Shared { id; node = target } ->
            let stub = shared_box target ~id ~ds_type in
            label, (stub, View.width stub / 2, View.width stub)
          | Child child -> label, tree child ~ds_type ~context ~name:None
          | Ref (structure : Replay.Structure.t) ->
            ( label
            , tree
                (Replay.Structure.current_root structure)
                ~ds_type:structure.snapshot.ds_type
                ~context
                ~name:(Some (structure_name structure)) ))
      in
      (* siblings in field order, each one [sibling_gap] past the last *)
      let (_ : int), placed =
        List.fold_map
          rendered
          ~init:0
          ~f:(fun x (label, (view, center, subtree_width)) ->
            ( x + subtree_width + sibling_gap
            , (label, view, x, x + center, subtree_width) ))
      in
      let centers =
        List.map
          placed
          ~f:
            (fun
              ((_ : string), (_ : View.t), (_ : int), center, (_ : int)) ->
            center)
      in
      let leftmost = List.hd_exn centers in
      let rightmost = List.last_exn centers in
      let midpoint = (leftmost + rightmost) / 2 in
      (* center the box over its children; where the box is wider than they
         spread, shift them right instead *)
      let parent_x = Int.max 0 (midpoint - (box_width / 2)) in
      let shift = Int.max 0 ((box_width / 2) - midpoint) in
      let centers = List.map centers ~f:(fun center -> center + shift) in
      let parent_center = parent_x + (box_width / 2) in
      let labeled_centers =
        List.zip_exn
          centers
          (List.map
             placed
             ~f:
               (fun
                 (label, (_ : View.t), (_ : int), (_ : int), (_ : int)) ->
               label))
        |> List.filter_map ~f:(fun (center, label) ->
          match String.is_empty label with
          | true -> None
          | false -> Some (center, label))
      in
      let rail_rows =
        rail ~parent_center ~centers
        ::
        (match List.is_empty labeled_centers with
         | true -> []
         | false -> [ rail_labels ~labeled_centers ])
      in
      let box_height = View.height box in
      let children_y = box_height + List.length rail_rows in
      let children =
        List.map
          placed
          ~f:(fun ((_ : string), view, x, (_ : int), (_ : int)) ->
            View.pad ~l:(x + shift) ~t:children_y view)
      in
      let width =
        List.fold
          placed
          ~init:(parent_x + box_width)
          ~f:
            (fun
              widest
              ((_ : string), (_ : View.t), x, (_ : int), subtree_width)
            -> Int.max widest (x + shift + subtree_width))
      in
      ( View.zcat
          ((View.pad ~l:parent_x box
            :: List.mapi rail_rows ~f:(fun index row ->
              View.pad ~t:(box_height + index) row))
           @ children)
      , parent_center
      , width )
  ;;

  (* One structure's whole tree, references followed, and how many nodes that
     came to. The structure counts as drawn before the walk starts, so a
     payload pointing back at its own root reads as a pointer rather than
     recursing forever.

     The count is the boxes actually drawn rather than
     {!Jsip_types.Snapshot.Node.fold}'s: the wire is a delta, so a
     structure's own snapshot may define three nodes and reference five more
     that a version before it defined. The diagram draws all eight, and
     saying "3 nodes" over eight boxes would be a plain lie. *)
  let canvas
    (structure : Replay.Structure.t)
    ~structures
    ~nodes
    ~new_addresses
    =
    let context = Context.create ~structures ~nodes ~new_addresses in
    Hash_set.add context.drawn structure.id;
    let view, (_ : int), (_ : int) =
      tree
        (Replay.Structure.current_root structure)
        ~ds_type:structure.snapshot.ds_type
        ~context
        ~name:(Some (structure_name structure))
    in
    view, Hash_set.length context.drawn_nodes
  ;;

  (* The pop-out itself: the diagram in a bordered slab over the panes. The
     screen is one surface everywhere else, so a floating thing has to say so
     twice — a shade above the rest, and a box around it — or it reads as a
     pane with an odd frame rather than as something on top. *)
  let view
    ~(structure : Replay.Structure.t)
    ~structures
    ~nodes
    ~new_addresses
    ~width
    ~height
    ~scroll
    ~pan
    =
    let inner_width = Int.max 3 (width - 2) in
    let inner_height = Int.max 2 (height - 2) in
    let canvas, drawn = canvas structure ~structures ~nodes ~new_addresses in
    let body_width = Panel.inner_width ~width:inner_width in
    let body_height = Int.max 1 (inner_height - Panel.header_height) in
    (* A tree is centered over its children all the way up, so the drawing
       has a middle and the slab should agree with it. Only where it fits, in
       both directions independently: past that the padding is zero and
       [scroll] and [pan] take over from the top-left corner, which is where
       reading a diagram too big for its slab has to start. *)
    let canvas =
      View.pad
        ~l:(Int.max 0 ((body_width - View.width canvas) / 2))
        ~t:(Int.max 0 ((body_height - View.height canvas) / 2))
        canvas
    in
    let scroll = clamp scroll ~max:(View.height canvas - body_height) in
    let pan = clamp pan ~max:(View.width canvas - body_width) in
    (* the name goes in the meta rather than in the title: a title names a
       pane and reads uppercased, and [bigger] is the program's word, not
       ours *)
    let meta =
      [%string
        "%{structure_name structure} · %{structure_type structure} · \
         %{node_count_label drawn} · esc back"]
    in
    let body =
      Panel.view
        ~bg:Theme.raised
        ~title:"diagram"
        ~meta
        ~width:inner_width
        ~height:inner_height
        (View.crop ~t:scroll ~l:pan canvas)
    in
    let edge = [ Theme.fg Theme.border; Attr.bg Theme.raised ] in
    let horizontal = Panel.repeat "─" ~width:inner_width in
    let side =
      View.vcat
        (List.init inner_height ~f:(fun (_ : int) ->
           View.text ~attrs:edge "│"))
    in
    View.vcat
      [ View.text ~attrs:edge [%string "┌%{horizontal}┐"]
      ; View.hcat [ side; body; side ]
      ; View.text ~attrs:edge [%string "└%{horizontal}┘"]
      ]
  ;;
end
