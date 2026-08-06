open! Core
open Jsip_types
open Jsip_replay

module Span = struct
  module Kind = struct
    type t =
      | Key
      | Value
      | Label
      | Arrow
      | Null
      | Gap
    [@@deriving sexp_of, equal]
  end

  type t = Kind.t * string [@@deriving sexp_of, equal]

  let text spans = String.concat (List.map spans ~f:snd)
end

module Row = struct
  type t =
    { depth : int
    ; guide : string
    ; field : string
    ; name : string
    ; ty : string
    ; value : Span.t list
    ; stats : string
    ; address : Snapshot.Address.t option
    ; fold : Heap_scene.Fold_key.t
    ; foldable : bool
    ; folded : bool
    ; hidden : int
    ; is_new : bool
    ; is_current : bool
    ; is_pointer : bool
    ; faded : bool
    ; matched : bool
    }
  [@@deriving sexp_of]

  let glyph t =
    match t.foldable with
    | false -> None
    | true -> Some (match t.folded with true -> "▸" | false -> "▾")
  ;;

  (* the columns that are there, two spaces apart and none trailing the last
     — the TUI pane's line, minus the terminal's colors *)
  let text t =
    let hidden =
      match t.folded && t.hidden > 0 with
      | false -> ""
      | true -> [%string "⋯ %{t.hidden#Int}"]
    in
    let tag = match t.is_new with true -> "new" | false -> "" in
    let columns =
      List.filter
        [ t.field; t.name; t.ty; Span.text t.value; t.stats; hidden; tag ]
        ~f:(fun column -> not (String.is_empty column))
    in
    (* the glyph column is two cells whether or not this row has a glyph *)
    let glyph =
      match glyph t with Some glyph -> [%string "%{glyph} "] | None -> "  "
    in
    [%string "%{t.guide}%{glyph}%{String.concat columns ~sep:\"  \"}"]
  ;;
end

(* ── ported outline vocabulary ───────────────────────────────────────── The
   readings below mirror {!Jsip_tui_components.Heap_pane}'s, whose comments
   are their long form. They cannot be shared — that library links
   [bonsai_term], which js_of_ocaml cannot compile — and {!Heap_scene}
   restates the same wire facts for the diagram's boxes. *)

let is_positional label = String.for_all label ~f:Char.is_digit

(* A row is a line, so everything printed on one has to be: the compiler
   prints a long inferred type across several lines and a walked string can
   hold anything at all. *)
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

(* the classic two-field payloads, by the labels the walker gives them: a map
   node's key and data, a hashtable entry's, a hash queue pair's *)
let binding_pairs = [ "v", "d"; "k", "v"; "key", "data" ]

let is_binding pair =
  List.mem binding_pairs pair ~equal:[%equal: string * string]
;;

(* the label a container puts its own size under — what tells a record OVER
   the contents from one OF them, in {!root_is_entry} *)
let counter_labels = [ "length"; "size"; "len"; "num_readers" ]
let is_counter label = List.mem counter_labels label ~equal:String.equal

(* a positional [Int 0] is an array's empty slot; printing fifteen of them
   buries the one that is set — the same trade the TUI pane makes *)
let printable_leaves leaves =
  List.filter leaves ~f:(fun (label, block) ->
    match is_positional label, (block : Snapshot.Block.t) with
    | true, Int 0 -> false
    | (true | false), _ -> true)
;;

(* What a row says where the wire gave it nothing to say: an unresolved
   revisit stub, a container whose entries are all empty slots. *)
let null_span = Span.Kind.Null, "null"

(* What a node says about itself, a LINE per field: [key → data] for the
   known binding pairs, [length n] for a counter, the bare value where there
   is only one, positional values joined where the labels are an array's, and
   [label=value] otherwise — user records included. *)
let field_lines leaves ~arity : Span.t list list =
  let kept = printable_leaves leaves in
  let fields =
    List.map kept ~f:(fun (label, block) ->
      label, one_line (Snapshot.Block.display block))
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
    [ [ Span.Kind.Label, "slots "; Value, Int.to_string arity ] ]
  | [] -> [ [ null_span ] ]
  | fields when positional ->
    [ [ Span.Kind.Value, String.concat (List.map fields ~f:snd) ~sep:", " ] ]
  | [ (key_label, key); (data_label, data) ]
    when is_binding (key_label, data_label) ->
    [ [ Span.Kind.Key, key; Arrow, " → "; Value, data ] ]
  | [ (label, value) ] when is_counter label ->
    [ [ Span.Kind.Label, [%string "%{label} "]; Value, value ] ]
  | [ ((_ : string), value) ] -> [ [ Span.Kind.Value, value ] ]
  | fields ->
    List.map fields ~f:(fun (label, value) ->
      [ Span.Kind.Label, [%string "%{label}="]; Value, value ])
;;

(* The same thing on one line, because a row IS a line: a record reads
   [a=1  b=2] across it rather than down. *)
let summary_spans leaves ~arity =
  field_lines leaves ~arity
  |> List.intersperse ~sep:[ Span.Kind.Gap, "  " ]
  |> List.concat
;;

(* whether the row's payload is a key-and-data pair — what makes a container
   count its rows as bindings rather than as elements *)
let is_binding_payload leaves =
  match printable_leaves leaves with
  | [ ((key_label : string), (_ : Snapshot.Block.t))
    ; ((data_label : string), (_ : Snapshot.Block.t))
    ] ->
    is_binding (key_label, data_label)
  | (_ : (string * Snapshot.Block.t) list) -> false
;;

(* What the row over there is showing, so a pointer at it names the same
   thing it does. Deliberately not {!node_edges}: that claims references as
   it goes, and this is a second look at a node someone else is drawing. *)
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
   structure's row counts its contents or summarizes them. A stdlib map's
   root IS an AVL node; a queue's [{length; first; last}] is a record over
   its cells, and [length 2] is exactly what its row has to say. What tells
   them apart is what the root holds of its own: a size counter and nothing
   else belongs to a record, anything else to an entry. *)
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
  (* no container keeps its own length inside one of its entries — which is
     what stops an EMPTY Core map from reading as an entry *)
  let counter_only =
    match payload with
    | [ (label, (_ : Snapshot.Block.t)) ] -> is_counter label
    | (_ : (string * Snapshot.Block.t) list) -> false
  in
  (not (List.is_empty payload)) && (not counter_only) && threads_itself
;;

let count_nodes (structure : Replay.Structure.t) =
  Snapshot.Node.fold
    structure.snapshot.root_node
    ~init:0
    ~f:(fun n (_ : Snapshot.Node.t) -> n + 1)
;;

let node_count_label count =
  match count with 1 -> "1 node" | count -> [%string "%{count#Int} nodes"]
;;

(* loosely 1024-based, one decimal past a kilobyte — the point is scale, and
   the words come off the wire's 64-bit runs *)
let bytes_label words =
  let bytes = words * 8 in
  match bytes < 1024, bytes < 1024 * 1024 with
  | true, (_ : bool) -> [%string "%{bytes#Int} B"]
  | false, true -> Printf.sprintf "%.1f kB" (Float.of_int bytes /. 1024.)
  | false, false ->
    Printf.sprintf "%.1f MB" (Float.of_int bytes /. (1024. *. 1024.))
;;

(* why a structure's row is drawn faded — said in words as well as in color,
   since "which of these three [m]s is the live one" is the question the
   fading exists to answer *)
let visibility_note (visibility : Replay.Visibility.t) =
  match visibility with
  | In_scope | Unknown -> None
  | Shadowed -> Some "shadowed"
  | Out_of_scope -> Some "out of scope"
;;

let structure_name (structure : Replay.Structure.t) =
  Replay.Structure.display structure
;;

(* the wire's own printed type is what a reader recognizes — [int M.t],
   [int list] — so it wins where the event stated one; the walked kind stands
   in where it did not *)
let structure_type (structure : Replay.Structure.t) =
  match structure.ty with
  | Some ty -> one_line ty.Type_info.printed
  | None -> Snapshot.Ds_type.display structure.snapshot.ds_type
;;

(* ── the walk ────────────────────────────────────────────────────────── *)

(* everything reference-following needs while a step's outline is built: the
   registry, which structures are already on it, and what the filter is
   looking for. Only [Id] blocks reference tracked structures — an [Address]
   block is a pointer the walker chose not to decode. *)
module Context = struct
  type t =
    { by_id : Replay.Structure.t Int.Map.t
    ; nodes : Replay.Nodes.t
    ; drawn : Int.Hash_set.t (** structures already placed in the outline *)
    ; drawn_nodes : Int.Hash_set.t
    (** node ids already listed: the wire shares blocks between structures
        and versions, so a second occurrence points at the first *)
    ; new_addresses : Snapshot.Address.Set.t
    ; query : string (** the filter, lowercased and stripped *)
    }

  let create ~structures ~nodes ~new_addresses ~filter =
    { by_id =
        Int.Map.of_alist_reduce
          (List.map structures ~f:(fun (structure : Replay.Structure.t) ->
             structure.id, structure))
          ~f:(fun first (_ : Replay.Structure.t) -> first)
    ; nodes
    ; drawn = Int.Hash_set.create ()
    ; drawn_nodes = Int.Hash_set.create ()
    ; new_addresses
    ; query = String.lowercase (String.strip filter)
    }
  ;;

  let id_of (block : Snapshot.Block.t) =
    match block with
    | Id id -> Some id
    | Address _ | Int _ | Float _ | String _ | Int32 _ | Int64 _
    | Nativeint _ | Float_array _ | Child ->
      None
  ;;

  let node t block = Option.bind (id_of block) ~f:(Replay.Nodes.find t.nodes)
  let structure t block = Option.bind (id_of block) ~f:(Map.find t.by_id)

  let matches t text =
    String.is_empty t.query
    || String.is_substring (String.lowercase text) ~substring:t.query
  ;;
end

module Edge = struct
  type t =
    | Nil (** an empty interior slot *)
    | Child of Snapshot.Node.t
    | Ref of Replay.Structure.t
    | Shared of
        { id : int
        ; node : Snapshot.Node.t option
        }
end

(* Read a node the way the wire writes it: [block] holds every kept field
   under its own label and in field order, a field holding a walked block
   reads [Child] and stands for the next node in [children], and an [Id]
   names a node defined earlier — a nested structure the first time, a
   pointer after. The only per-structure knowledge left is
   {!Snapshot.Ds_type.interior_labels}, which says which [Int 0] is an empty
   pointer rather than the number. *)
let node_edges (node : Snapshot.Node.t) ~ds_type ~(context : Context.t) =
  let children = Queue.of_list node.children in
  let interior = Snapshot.Ds_type.interior_labels ds_type in
  let is_interior label = List.mem interior label ~equal:String.equal in
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

(* One entry of the outline before it is flattened into rows: what reached
   it, what it is, what it holds, and what hangs under it. Folds are applied
   afterwards, in {!assemble}, so what nests where never depends on what
   happens to be folded. *)
module Entry = struct
  type t =
    { field : string
    ; name : string
    ; ty : string
    ; stats : string
    ; value : Span.t list
    ; address : Snapshot.Address.t option
    ; is_new : bool
    ; is_current : bool
    ; is_binding : bool
    ; is_pointer : bool
    ; faded : bool
    (** the owning structure's verdict, decided at its entry — which is what
        lets a nested [Ref] structure bring its own *)
    ; matched : bool
    ; fold : Heap_scene.Fold_key.t
    ; owns_fold : bool
    (** false where the structure's own row already carries this box's fold:
        one box, one glyph *)
    ; children : t list (** built even when folded, so counts are known *)
    }

  let rec size t =
    List.sum (module Int) t.children ~f:(fun child -> 1 + size child)
  ;;

  let rec exists t ~f =
    f t || List.exists t.children ~f:(fun child -> exists child ~f)
  ;;

  (* a structure whose header matches the filter keeps its whole subtree lit,
     exactly as the canvas relights a matching root's boxes *)
  let rec relight t =
    { t with matched = true; children = List.map t.children ~f:relight }
  ;;
end

(* what the filter reads on a row: everything it says, plus the address, so
   [/0x7f] finds a box by where it lives *)
let searchable ~field ~name ~ty ~value ~stats ~address =
  String.concat
    ~sep:" "
    [ field
    ; name
    ; ty
    ; Span.text value
    ; stats
    ; Option.value_map address ~default:"" ~f:Snapshot.Address.display
    ]
;;

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

(* One node's entries, at the level it was reached on. Three rules turn a
   walked heap shape into an outline, and none of them needs to know which
   container it is looking at:

   - a node with nothing of its own to print is plumbing — a hashtable's
     bucket array, a wrapper record — so it gets no row and the things it
     points at take its place, here;
   - an edge whose label is the structure's own skeleton carries on through
     the container rather than descending into it, so what it reaches lists
     as a SIBLING;
   - so does an edge landing on a node shaped exactly like this one — the
     tail of a cons cell, the subtree of a map node.

   Everything else is content, and nests. *)
let rec entries_of
  (node : Snapshot.Node.t)
  ~field
  ~ds_type
  ~(context : Context.t)
  ~faded
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
       subtree: where it sits is all that changed *)
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
        ~faded
        ~structure_id
        ~path
    | Ref structure ->
      (* another structure, listed here rather than in a section of its own —
         so it brings its own verdict with it, and a live map hanging off a
         faded queue cell stays lit *)
      [ structure_entry structure ~field:(shown label) ~context ]
    | Shared { id; node = target } ->
      [ pointer_entry
          ~id
          ~target
          ~field:(shown label)
          ~ds_type
          ~faded
          ~context
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
    let address = Some node.virtual_address in
    { Entry.field
    ; name = ""
    ; ty = ""
    ; stats = ""
    ; value
    ; address
    ; is_new = Set.mem context.new_addresses node.virtual_address
    ; is_current = false
    ; is_binding
    ; is_pointer = false
    ; faded
    ; matched =
        Context.matches
          context
          (searchable ~field ~name:"" ~ty:"" ~value ~stats:"" ~address)
    ; fold = { Heap_scene.Fold_key.structure_id; path }
    ; owns_fold = true
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
    let value = summary_spans printable ~arity:(List.length node.block) in
    (* A binding whose data is a block of its own keeps the key here and the
       data on the line below. Left alone the key would read as a leaf, so it
       says [→] instead: this is half a binding, the rest is under it. *)
    let value =
      match
        printable, List.filter edges ~f:(fun edge -> not (continues edge))
      with
      | ( [ (key_label, (_ : Snapshot.Block.t)) ]
        , [ (data_label, (_ : Edge.t)) ] )
        when is_binding (key_label, data_label) ->
        value @ [ Span.Kind.Arrow, " →" ]
      | (_ : (string * Snapshot.Block.t) list), (_ : (string * Edge.t) list)
        ->
        value
    in
    row ~value ~is_binding:(is_binding_payload leaves) ~children
    :: of_kind true

(* A tracked structure's own row: its name, its type, and either what its
   root record says or how much it holds. Its contents hang underneath.

   Built even when it will be folded away, because the reference claims made
   while walking it have to hold either way — a folded structure keeps the
   structures it references tucked inside it rather than spilling them back
   out as rows of their own. *)
and structure_entry (structure : Replay.Structure.t) ~field ~context =
  let ds_type = structure.snapshot.ds_type in
  let root = Replay.Structure.current_root structure in
  let faded = not (Replay.Visibility.is_reachable structure.visibility) in
  let root_fold = Heap_scene.Fold_key.root structure.id in
  let rows =
    entries_of
      root
      ~field:""
      ~ds_type
      ~context
      ~faded
      ~structure_id:structure.id
      ~path:[]
  in
  let value, children =
    match rows with
    | first :: rest
      when (not (root_is_entry root ~ds_type))
           && (not first.is_pointer)
           && [%equal: Snapshot.Address.t option]
                first.address
                (Some root.virtual_address) ->
      (* the root was a record over the contents: its own summary is the
         structure's, and its children are the structure's *)
      first.value, first.children @ rest
    | (_ : Entry.t list) ->
      ( [ Span.Kind.Label, count_label rows ]
      , (* the root's own row is the same box as this one, so the fold stays
           up here: one box, one glyph *)
        List.map rows ~f:(fun (row : Entry.t) ->
          match Heap_scene.Fold_key.equal row.fold root_fold with
          | true -> { row with Entry.owns_fold = false }
          | false -> row) )
  in
  (* size on the structure's own row, twice over — how many nodes, and how
     much memory their blocks pin. That row is all you see of a folded
     structure. *)
  let stats =
    [%string
      "%{node_count_label (count_nodes structure)} · %{bytes_label \
       (Snapshot.Node.heap_words structure.snapshot.root_node)}"]
  in
  (* the verdict rides the stats column, so the row says in words what the
     fading says in color *)
  let stats =
    match visibility_note structure.visibility with
    | None -> stats
    | Some note -> [%string "%{stats} · %{note}"]
  in
  let name = structure_name structure in
  let ty = structure_type structure in
  let address = Some structure.address in
  let entry =
    { Entry.field
    ; name
    ; ty
    ; stats
    ; value
    ; address
    ; is_new = Set.mem context.Context.new_addresses structure.address
    ; is_current = structure.is_current
    ; is_binding = false
    ; is_pointer = false
    ; faded
    ; matched =
        Context.matches
          context
          (searchable ~field ~name ~ty ~value ~stats ~address)
    ; fold = root_fold
    ; owns_fold = true
    ; children
    }
  in
  (* the filter finds STRUCTURES the way the canvas's does — by everything
     the header says, kind included, which a printed type column may not *)
  match
    (not (String.is_empty context.query))
    && Heap_scene.matches_filter structure ~filter:context.query
  with
  | true -> Entry.relight entry
  | false -> entry

(* A node the outline has already listed, pointed at rather than listed twice
   — which is the whole point of a persistent structure: two versions of a
   map share their subtrees, and repeating them would bury the one binding
   that differs. Named by what its target holds rather than by the wire's
   node number: [↗ "b" → 2] names something on screen, [↗ #11] names nothing. *)
and pointer_entry
  ~id
  ~target
  ~field
  ~ds_type
  ~faded
  ~(context : Context.t)
  ~structure_id
  ~path
  =
  let value =
    match target with
    | Some node -> (Span.Kind.Arrow, "↗ ") :: shared_spans node ~ds_type
    | None -> [ Span.Kind.Arrow, "↗ "; Label, [%string "#%{id#Int}"] ]
  in
  let address =
    Option.map target ~f:(fun (node : Snapshot.Node.t) ->
      node.virtual_address)
  in
  { Entry.field
  ; name = ""
  ; ty = ""
  ; stats = ""
  ; value
  ; address
  ; is_new = false
  ; is_current = false
  ; is_binding = false
  ; is_pointer = true
  ; faded
  ; matched =
      Context.matches
        context
        (searchable ~field ~name:"" ~ty:"" ~value ~stats:"" ~address)
  ; fold = { Heap_scene.Fold_key.structure_id; path }
  ; owns_fold = true
  ; children = []
  }
;;

(* Every live structure, in registry order. A structure referenced from
   another one nests inside its referrer instead of getting a row of its own
   — the registry still decides what is alive, only the placement moves. *)
let structure_entries ~structures ~(context : Context.t) =
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
      structure_entry structure ~field:"" ~context :: entries
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

(* The outline, one row per entry the folds leave visible. Guides are built
   on the way down: a run of [│  ]/[   ] for the ancestors, then this row's
   own elbow. A structure's row is the outline's top level and carries no
   guide. *)
let assemble entries ~folds =
  let rec walk (entries : Entry.t list) ~prefix ~depth acc =
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
      let foldable = entry.owns_fold && hidden > 0 in
      let folded = foldable && Set.mem folds entry.fold in
      let acc =
        { Row.depth
        ; guide
        ; field = entry.field
        ; name = entry.name
        ; ty = entry.ty
        ; value = entry.value
        ; stats = entry.stats
        ; address = entry.address
        ; fold = entry.fold
        ; foldable
        ; folded
        ; hidden
        ; is_new = entry.is_new
        ; is_current = entry.is_current
        ; is_pointer = entry.is_pointer
        ; faded = entry.faded
        ; matched = entry.matched
        }
        :: acc
      in
      match folded with
      | true -> acc
      | false ->
        walk entry.children ~prefix:continuation ~depth:(depth + 1) acc)
  in
  List.rev (walk entries ~prefix:"" ~depth:0 [])
;;

let rows
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~filter
  ~sort_by_address
  ~accordion
  =
  let structures =
    match sort_by_address with
    | false -> structures
    | true -> Heap_scene.by_address structures
  in
  let context = Context.create ~structures ~nodes ~new_addresses ~filter in
  let entries = structure_entries ~structures ~context in
  let current_id =
    List.find_map structures ~f:(fun (structure : Replay.Structure.t) ->
      match structure.is_current with
      | true -> Some structure.id
      | false -> None)
  in
  (* the accordion must not fold the walked structure away — including when
     it is listed INSIDE another structure, which is where a referenced one
     lives *)
  let folds =
    match accordion with
    | false -> folds
    | true ->
      List.fold entries ~init:folds ~f:(fun folds (entry : Entry.t) ->
        let contains_current =
          match current_id with
          | None -> false
          | Some id ->
            Entry.exists entry ~f:(fun (entry : Entry.t) ->
              entry.fold.structure_id = id)
        in
        match contains_current with
        | true -> folds
        | false -> Set.add folds entry.fold)
  in
  assemble entries ~folds
;;
