open! Core
open Jsip_types
open Jsip_parsing
open Jsip_replay
open Jsip_web_components

(* every dump here is a golden fixture — verbatim compiler output vendored
   under testing/expected/ (see testing/README.md) *)
let replay_of_fixture name =
  let parsed_info =
    Dump_reader.read [%string "../../../testing/expected/%{name}.dump"]
    |> Or_error.ok_exn
  in
  Replay.create (Call_stack.create ~parsed_info)
;;

let scene_at
  ?(folds = Set.empty (module Heap_scene.Fold_key))
  ?(filter = "")
  ?(sort_by_address = false)
  ?(accordion = false)
  replay
  ~step
  =
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  Heap_scene.build
    ~structures
    ~nodes
    ~new_addresses
    ~folds
    ~filter
    ~sort_by_address
    ~accordion
;;

(* one box per line, indented, the way the canvas nests them *)
let rec show_node ?(indent = 0) (node : Heap_scene.Node.t) ~edge =
  let pad = String.make indent ' ' in
  let edge = match String.is_empty edge with true -> "" | false -> [%string "%{edge}→ "] in
  let kind =
    match node.kind with
    | Heap_scene.Kind.Block -> ""
    | Heap_scene.Kind.Nil -> " [nil]"
    | Heap_scene.Kind.Shared id -> [%string " [↗%{id#Int}]"]
  in
  let tags =
    List.filter_opt
      [ (match node.is_new with true -> Some "new" | false -> None)
      ; (match node.faded with true -> Some "faded" | false -> None)
      ; (match node.folded with
         | true -> Some [%string "folded ⋯%{node.hidden_count#Int}"]
         | false -> None)
      ]
  in
  let tags =
    match tags with
    | [] -> ""
    | tags -> [%string " (%{String.concat tags ~sep:\", \"})"]
  in
  print_endline [%string "%{pad}%{edge}%{node.label}%{kind}%{tags}"];
  List.iter node.children ~f:(fun (edge, child) ->
    show_node child ~indent:(indent + 2) ~edge)
;;

let show_scene (roots, (stats : Heap_scene.Stats.t)) =
  List.iter roots ~f:(fun (root : Heap_scene.Root.t) ->
    let current = match root.is_current with true -> " ★" | false -> "" in
    print_endline [%string "▾ %{root.header} · %{root.count#Int} nodes%{current}"];
    show_node root.node ~indent:2 ~edge:"";
    print_endline "");
  print_s [%sexp (stats : Heap_scene.Stats.t)]
;;

let%expect_test "a nested map draws as the AVL tree it physically is" =
  let replay = replay_of_fixture "map_nested" in
  show_scene (scene_at replay ~step:1);
  [%expect {|
    ▾ #1 · map ⟨string ⇒ int⟩ · 1 nodes
      #1 · "inner" → 2

    ▾ #2 · map ⟨string ⇒ int⟩ · 2 nodes ★
      #2 · "inner" → 2 (new)
        l→ ∅ [nil]
        r→ "outer" → 1 (new)

    ((structures 2) (nodes 3) (new_nodes 2) (hits 0))
    |}]
;;

let%expect_test "folding a root tucks the whole structure behind its box" =
  let replay = replay_of_fixture "map_nested" in
  let folds =
    Set.singleton (module Heap_scene.Fold_key) (Heap_scene.Fold_key.root 1)
  in
  show_scene (scene_at replay ~folds ~step:1);
  [%expect {|
    ▾ #1 · map ⟨string ⇒ int⟩ · 1 nodes
      #1 · "inner" → 2

    ▾ #2 · map ⟨string ⇒ int⟩ · 2 nodes ★
      #2 · "inner" → 2 (new)
        l→ ∅ [nil]
        r→ "outer" → 1 (new)

    ((structures 2) (nodes 3) (new_nodes 2) (hits 0))
    |}]
;;

let%expect_test "the filter lights matching structures and dims the rest" =
  let replay = replay_of_fixture "queue_of_maps" in
  let roots, stats = scene_at replay ~filter:"queue" ~step:2 in
  List.iter roots ~f:(fun (root : Heap_scene.Root.t) ->
    print_endline
      [%string
        "%{root.header} — matched %{root.matched#Bool}"]);
  print_s [%sexp (stats : Heap_scene.Stats.t)];
  [%expect {|
    m · map ⟨string ⇒ int⟩ — matched false
    q · queue ⟨int M.t⟩ — matched true
    ((structures 2) (nodes 3) (new_nodes 1) (hits 3))
    |}]
;;

let%expect_test "a shared subtree draws once and points after" =
  (* map_spine_sharing: two versions of a map share their spines, so the
     second version's reference to the shared block must be a [↗] box *)
  let replay = replay_of_fixture "map_spine_sharing" in
  show_scene (scene_at replay ~step:(Replay.length replay - 1));
  [%expect {|
    ▾ m · map ⟨string ⇒ int⟩ · 5 nodes
      m · "f" → 6
        l→ "d" → 4
          l→ "b" → 2
          r→ ∅ [nil]
        r→ "h" → 8
          l→ ∅ [nil]
          r→ "j" → 10

    ▾ bigger · map ⟨string ⇒ int⟩ · 3 nodes ★
      bigger · "f" → 6 (new)
        l→ ↗ "d" → 4 [↗7]
        r→ "h" → 8 (new)
          l→ "g" → 7 (new)
          r→ ↗ "j" → 10 [↗11]

    ((structures 2) (nodes 8) (new_nodes 3) (hits 0))
    |}]
;;

let%expect_test "accordion folds every structure but the walked one" =
  let replay = replay_of_fixture "queue_of_maps" in
  let roots, (_ : Heap_scene.Stats.t) =
    scene_at replay ~accordion:true ~step:0
  in
  List.iter roots ~f:(fun (root : Heap_scene.Root.t) ->
    print_endline
      [%string
        "%{root.header} — folded %{root.node.folded#Bool} · current \
         %{root.is_current#Bool}"]);
  [%expect {| m · map ⟨string ⇒ int⟩ — folded false · current true |}]
;;
