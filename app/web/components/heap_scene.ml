open! Core
open Jsip_types
open Jsip_replay

module Fold_key = struct
  module T = struct
    type t =
      { structure_id : int
      ; path : int list
      }
    [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparator.Make (T)

  let root structure_id = { structure_id; path = [] }
end

module Kind = struct
  type t =
    | Block
    | Nil
    | Shared of int
  [@@deriving sexp_of, equal]
end

module Line = struct
  module Part = struct
    type t =
      | Key of string
      | Value of string
      | Label of string
      | Arrow
      | Null
    [@@deriving sexp_of, equal]
  end

  type t = Part.t list [@@deriving sexp_of, equal]

  let text parts =
    List.map parts ~f:(fun (part : Part.t) ->
      match part with
      | Key text | Value text | Label text -> text
      | Arrow -> " → "
      | Null -> "null")
    |> String.concat
  ;;
end

module Node = struct
  type t =
    { key : Fold_key.t
    ; kind : Kind.t
    ; address : Snapshot.Address.t option
    ; label : string
    ; lines : Line.t list
    ; raw : (string * string) list
    ; words : int
    ; name_tag : string option
    ; is_new : bool
    ; faded : bool
    ; matched : bool
    ; folded : bool
    ; hidden_count : int
    ; children : (string * t) list
    }
  [@@deriving sexp_of]

  let rec fold t ~init ~f =
    List.fold t.children ~init:(f init t) ~f:(fun acc ((_ : string), child) ->
      fold child ~init:acc ~f)
  ;;
end

module Root = struct
  type t =
    { structure_id : int
    ; header : string
    ; note : string option
    ; count : int
    ; words : int
    ; faded : bool
    ; matched : bool
    ; is_current : bool
    ; node : Node.t
    }
  [@@deriving sexp_of]
end

module Stats = struct
  type t =
    { structures : int
    ; nodes : int
    ; new_nodes : int
    ; hits : int
    }
  [@@deriving sexp_of, equal]
end

(* ── ported outline vocabulary ─────────────────────────────────────────
   The readings below mirror {!Jsip_tui_components.Heap_pane}'s: the same
   wire facts drawn as boxes instead of rows. They cannot share code — the
   TUI library links [bonsai_term], which js_of_ocaml cannot — so the rules
   are restated here; [Heap_pane]'s comments are the long-form versions. *)

let is_positional label = String.for_all label ~f:Char.is_digit

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

let binding_pairs = [ "v", "d"; "k", "v"; "key", "data" ]

let is_binding pair =
  List.mem binding_pairs pair ~equal:[%equal: string * string]
;;

let counter_labels = [ "length"; "size"; "len"; "num_readers" ]
let is_counter label = List.mem counter_labels label ~equal:String.equal

(* a positional [Int 0] is an array's empty slot; printing fifteen of them
   buries the one that is set — the same trade the outline makes *)
let printable_leaves leaves =
  List.filter leaves ~f:(fun (label, block) ->
    match is_positional label, (block : Snapshot.Block.t) with
    | true, Int 0 -> false
    | (true | false), _ -> true)
;;

(* what a node says about itself, a line per field — [key → data] for the
   known binding pairs, [slots n] for an all-empty array, the bare value
   where there is only one *)
let field_lines leaves ~arity : Line.t list =
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
    [ [ Line.Part.Label "slots "; Value (Int.to_string arity) ] ]
  | [] -> [ [ Line.Part.Null ] ]
  | fields when positional ->
    [ [ Line.Part.Value (String.concat (List.map fields ~f:snd) ~sep:", ") ]
    ]
  | [ (key_label, key); (data_label, data) ]
    when is_binding (key_label, data_label) ->
    [ [ Line.Part.Key key; Arrow; Value data ] ]
  | [ (label, value) ] when is_counter label ->
    [ [ Line.Part.Label [%string "%{label} "]; Value value ] ]
  | [ ((_ : string), value) ] -> [ [ Line.Part.Value value ] ]
  | fields ->
    List.map fields ~f:(fun (label, value) ->
      [ Line.Part.Label [%string "%{label}="]; Value value ])
;;

(* a second look at a node someone else draws — pointer boxes name what
   their target holds, so this must not claim references *)
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

let visibility_note (visibility : Replay.Visibility.t) =
  match visibility with
  | In_scope | Unknown -> None
  | Shadowed -> Some "shadowed"
  | Out_of_scope -> Some "out of scope"
;;

let structure_name (structure : Replay.Structure.t) =
  Replay.Structure.display structure
;;

let structure_type (structure : Replay.Structure.t) =
  let kind = Snapshot.Ds_type.display structure.snapshot.ds_type in
  match structure.ty with
  | None -> kind
  | Some ty -> [%string "%{kind} %{Type_info.display ty}"]
;;

let header_text (structure : Replay.Structure.t) =
  let label =
    [%string "%{structure_name structure} · %{structure_type structure}"]
  in
  match visibility_note structure.visibility with
  | None -> label
  | Some note -> [%string "%{label} · %{note}"]
;;

let matches_filter structure ~filter =
  match String.is_empty filter with
  | true -> true
  | false ->
    String.is_substring
      (String.lowercase (header_text structure))
      ~substring:(String.lowercase filter)
;;

let by_address structures =
  List.sort
    structures
    ~compare:(fun (a : Replay.Structure.t) (b : Replay.Structure.t) ->
      Snapshot.Address.compare a.address b.address)
;;

(* ── the walk ────────────────────────────────────────────────────────── *)

module Context = struct
  type t =
    { by_id : Replay.Structure.t Int.Map.t
    ; nodes : Replay.Nodes.t
    ; drawn : Int.Hash_set.t
    ; drawn_nodes : Int.Hash_set.t
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

module Edge = struct
  type t =
    | Nil
    | Child of Snapshot.Node.t
    | Ref of Replay.Structure.t
    | Shared of
        { id : int
        ; node : Snapshot.Node.t option
        }
end

(* read a node the way the wire writes it: labeled fields in order, [Child]
   standing for the next entry of [children], [Id] naming a node defined
   earlier — a nested structure the first time, a pointer after *)
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
              { id = structure.id
              ; node = Some structure.snapshot.root_node
              }))
    | None ->
      (match Context.node context block with
       | None -> None
       | Some (definition : Snapshot.Node.t) ->
         (match Hash_set.mem context.drawn_nodes definition.id with
          | false -> Some (Edge.Child definition)
          | true ->
            Some
              (Edge.Shared { id = definition.id; node = Some definition })))
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
        | Int 0 when is_interior label -> (label, Edge.Nil) :: edges, leaves
        | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
        | Float_array _ | Address _ ->
          edges, (label, block) :: leaves)
  in
  let unclaimed =
    List.map (Queue.to_list children) ~f:(fun child -> "", Edge.Child child)
  in
  List.rev edges @ unclaimed, List.rev leaves
;;

(* The machine-level rows detail tier 3 adds under the fields: the address,
   the header word, and the first fields' actual words — a pointer field's
   target address, an immediate's tagged word ([2n + 1], which is also what
   an empty slot's "0" really holds), a boxed payload named for what it is.
   The last 14 bits, like the design mockup: enough to see the tag bit. *)
let raw_rows (node : Snapshot.Node.t) ~(context : Context.t) =
  let arity = List.length node.block in
  let bits value =
    let tagged = ((value * 2) + 1) land 0x3fff in
    let buffer = Bytes.make 14 '0' in
    for bit = 0 to 13 do
      match (tagged lsr bit) land 1 with
      | 1 -> Bytes.set buffer (13 - bit) '1'
      | (_ : int) -> ()
    done;
    [%string "0b%{Bytes.to_string buffer}"]
  in
  let children = Queue.of_list node.children in
  let field_word ((_ : string), (block : Snapshot.Block.t)) =
    match block with
    | Child ->
      Queue.dequeue children
      |> Option.map ~f:(fun (child : Snapshot.Node.t) ->
        Snapshot.Address.display child.virtual_address)
    | Id (_ : int) ->
      (match Context.structure context block with
       | Some structure -> Some (Snapshot.Address.display structure.address)
       | None ->
         Context.node context block
         |> Option.map ~f:(fun (target : Snapshot.Node.t) ->
           Snapshot.Address.display target.virtual_address))
    | Int value -> Some (bits value)
    | Float (_ : float) -> Some "boxed float"
    | Float_array floats ->
      Some [%string "boxed floats ×%{List.length floats#Int}"]
    | String text -> Some [%string "ptr · %{String.length text#Int}B chars"]
    | Int32 (_ : int32) | Int64 (_ : int64) | Nativeint (_ : nativeint) ->
      Some "boxed word"
    | Address address -> Some (Snapshot.Address.display address)
  in
  [ "@", Snapshot.Address.display node.virtual_address
  ; "hd", [%string "%{arity + 1#Int}w · %{(arity + 1) * 8#Int}B"]
  ]
  @ List.filter_mapi node.block ~f:(fun index field ->
    match index < 6 with
    | false -> None
    | true ->
      Option.map (field_word field) ~f:(fun word ->
        [%string "[%{index#Int}]"], word))
;;

(* One structure's tree of boxes, in the diagram's reading: every walked
   block a box, [∅] boxes for empty skeleton slots, dashed [↗] boxes for
   nodes already drawn (sharing, cycles), and a referenced structure's whole
   tree inlined with its own name and verdict. Folds are NOT applied here —
   {!prune} takes them off afterwards — so what claims what never depends on
   what happens to be folded, and folding a subtree cannot spill the
   structures it references back out as roots of their own. *)
let rec tree
  (node : Snapshot.Node.t)
  ~ds_type
  ~(context : Context.t)
  ~faded
  ~name
  ~structure_id
  ~path
  ~query
  : Node.t
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
  let lines = field_lines leaves ~arity:(List.length node.block) in
  let raw = raw_rows node ~context in
  let key = { Fold_key.structure_id; path = List.rev path } in
  let child_key index =
    { Fold_key.structure_id; path = List.rev (index :: path) }
  in
  let children =
    List.mapi edges ~f:(fun index (label, (edge : Edge.t)) ->
      match edge with
      | Nil ->
        ( label
        , { Node.key = child_key index
          ; kind = Kind.Nil
          ; address = None
          ; label = "∅"
          ; lines = []
          ; raw = []
          ; words = 0
          ; name_tag = None
          ; is_new = false
          ; faded
          ; matched = String.is_empty query
          ; folded = false
          ; hidden_count = 0
          ; children = []
          } )
      | Shared { id; node = target } ->
        let lines =
          match target with
          | Some target ->
            field_lines
              (shared_leaves target ~ds_type)
              ~arity:(List.length target.block)
          | None -> [ [ Line.Part.Label [%string "#%{id#Int}"] ] ]
        in
        let summary =
          Option.value_map (List.hd lines) ~default:"" ~f:Line.text
        in
        let label_text = [%string "↗ %{summary}"] in
        let address =
          Option.map target ~f:(fun (target : Snapshot.Node.t) ->
            target.virtual_address)
        in
        ( label
        , { Node.key = child_key index
          ; kind = Kind.Shared id
          ; address
          ; label = label_text
          ; lines
          ; raw = []
          ; words = 0
          ; name_tag = None
          ; is_new = false
          ; faded
          ; matched =
              String.is_empty query
              || String.is_substring
                   (String.lowercase label_text)
                   ~substring:query
          ; folded = false
          ; hidden_count = 0
          ; children = []
          } )
      | Child child ->
        ( label
        , tree
            child
            ~ds_type
            ~context
            ~faded
            ~name:None
            ~structure_id
            ~path:(index :: path)
            ~query )
      | Ref structure ->
        (* another structure drawn here rather than under a header of its
           own — it keeps its own verdict, so a live map hanging off a faded
           queue cell stays lit. Its folds key on its own id, so they follow
           it whichever structure it ends up drawn inside. *)
        ( label
        , tree
            (Replay.Structure.current_root structure)
            ~ds_type:structure.snapshot.ds_type
            ~context
            ~faded:(not (Replay.Visibility.is_reachable structure.visibility))
            ~name:(Some (structure_name structure))
            ~structure_id:structure.id
            ~path:[]
            ~query ))
  in
  let label =
    let summary = Option.map (List.hd lines) ~f:Line.text in
    match name, summary with
    | Some name, Some summary -> [%string "%{name} · %{summary}"]
    | Some name, None -> name
    | None, Some summary -> summary
    | None, None -> "·"
  in
  let search =
    String.lowercase
      (String.concat
         ~sep:" "
         (label
          :: Snapshot.Address.display node.virtual_address
          :: List.map lines ~f:Line.text))
  in
  { Node.key
  ; kind = Kind.Block
  ; address = Some node.virtual_address
  ; label
  ; lines
  ; raw
  ; words = List.length node.block + 1
  ; name_tag = name
  ; is_new = Set.mem context.new_addresses node.virtual_address
  ; faded
  ; matched =
      String.is_empty query || String.is_substring search ~substring:query
  ; folded = false
  ; hidden_count = 0
  ; children
  }
;;

(* folds as a post-pass: a folded node keeps its box and hands its subtree
   to the [⋯ n] count. [accordion] folds every structure's root but the one
   the replay is standing in — node folds inside the open one keep working,
   exactly the TUI's accordion contract. *)
let rec prune (node : Node.t) ~folds : Node.t =
  match Set.mem folds node.key && not (List.is_empty node.children) with
  | true ->
    { node with
      folded = true
    ; hidden_count = List.length node.children
    ; children = []
    }
  | false ->
    { node with
      children =
        List.map node.children ~f:(fun (label, child) ->
          label, prune child ~folds)
    }
;;

let build
  ~structures
  ~nodes
  ~new_addresses
  ~folds
  ~filter
  ~sort_by_address
  ~accordion
  =
  let query = String.lowercase (String.strip filter) in
  let structures =
    match sort_by_address with
    | false -> structures
    | true -> by_address structures
  in
  let context = Context.create ~structures ~nodes ~new_addresses in
  let roots =
    List.filter_map structures ~f:(fun (structure : Replay.Structure.t) ->
      match Hash_set.mem context.drawn structure.id with
      | true -> None (* already inlined under an earlier referrer *)
      | false ->
        Hash_set.add context.drawn structure.id;
        let faded = not (Replay.Visibility.is_reachable structure.visibility) in
        let node =
          tree
            (Replay.Structure.current_root structure)
            ~ds_type:structure.snapshot.ds_type
            ~context
            ~faded
            ~name:(Some (structure_name structure))
            ~structure_id:structure.id
            ~path:[]
            ~query
        in
        let folds =
          match accordion && not structure.is_current with
          | false -> folds
          | true -> Set.add folds (Fold_key.root structure.id)
        in
        let node = prune node ~folds in
        let count =
          Node.fold node ~init:0 ~f:(fun count (child : Node.t) ->
            match child.kind with
            | Kind.Block -> count + 1
            | Kind.Nil | Kind.Shared (_ : int) -> count)
        in
        let header = header_text structure in
        (* a structure whose header matches keeps its whole tree lit: the
           filter finds structures the way the outline's [/] does, and
           node-level matches light individual boxes on top of that *)
        let header_hit =
          (not (String.is_empty query))
          && String.is_substring (String.lowercase header) ~substring:query
        in
        let node =
          match header_hit with
          | false -> node
          | true ->
            let rec relight (node : Node.t) =
              { node with
                matched = true
              ; children =
                  List.map node.children ~f:(fun (label, child) ->
                    label, relight child)
              }
            in
            relight node
        in
        let matched =
          String.is_empty query
          || Node.fold node ~init:false ~f:(fun hit (child : Node.t) ->
            hit || child.matched)
        in
        Some
          { Root.structure_id = structure.id
          ; header
          ; note = visibility_note structure.visibility
          ; count
          ; words = Snapshot.Node.heap_words structure.snapshot.root_node
          ; faded
          ; matched
          ; is_current = structure.is_current
          ; node
          })
  in
  let stats =
    let nodes, new_nodes, hits =
      List.fold roots ~init:(0, 0, 0) ~f:(fun acc (root : Root.t) ->
        Node.fold
          root.node
          ~init:acc
          ~f:(fun (nodes, new_nodes, hits) node ->
            ( (match node.kind with
               | Kind.Block -> nodes + 1
               | Kind.Nil | Kind.Shared (_ : int) -> nodes)
            , (match node.is_new with
               | true -> new_nodes + 1
               | false -> new_nodes)
            , match (not (String.is_empty query)) && node.matched with
              | true -> hits + 1
              | false -> hits )))
    in
    { Stats.structures = List.length roots; nodes; new_nodes; hits }
  in
  roots, stats
;;
