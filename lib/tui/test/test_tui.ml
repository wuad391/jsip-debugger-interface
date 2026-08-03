open! Core
open Jsip_types
open Jsip_parsing
open Jsip_replay
open Jsip_tui

(* render a view the way a colorless terminal would, for readable expects *)
let print_view ?(width = 60) ?(height = 14) view =
  let image = Bonsai_term.View.Private.notty_image view in
  let buffer = Buffer.create 1024 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (width, height) image;
  Buffer.contents buffer
  |> String.split_lines
  |> List.map ~f:String.rstrip
  |> List.rev
  |> List.drop_while ~f:String.is_empty
  |> List.rev
  |> String.concat ~sep:"\n"
  |> print_endline
;;

(* every dump here is a golden fixture — verbatim compiler output vendored
   under testing/expected/ (see testing/README.md) *)
let replay_of_fixture name =
  let parsed_info = Queue.create () in
  Dump_reader.read_until_empty
    [%string "../../../testing/expected/%{name}.dump"]
    ~store_data:(Queue.enqueue parsed_info);
  Replay.create (Call_stack.create ~parsed_info)
;;

let heap_view ?(width = 56) ?(height = 15) replay ~step =
  let { Replay.Step.structures; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  print_view
    ~width
    ~height
    (Heap_pane.view
       ~width
       ~height
       ~structures
       ~new_addresses
       ~folds:(Set.empty (module Heap_pane.Fold))
       ~scroll:0)
;;

let calls_of replay =
  Array.init (Replay.length replay) ~f:(fun step ->
    (Replay.step_exn replay ~step).call)
;;

let live_of replay ~step =
  let { Replay.Step.frames; _ } = Replay.step_exn replay ~step in
  List.map frames ~f:(fun (frame : Call.t) -> fst frame.range)
;;

let%expect_test "stack pane: every call visible, the live chain lit" =
  (* at map_fold's step 2 the callback's [M.add] runs inside the fold: both
     live rows render bright, the rest of the run stays dimmed but listed,
     and long argument lists wrap *)
  let replay = replay_of_fixture "map_fold" in
  print_view
    ~height:10
    (Stack_pane.view
       ~width:56
       ~height:10
       ~calls:(calls_of replay)
       ~live:(live_of replay ~step:2)
       ~selected:1
       ~folds:Int.Set.empty);
  [%expect
    {|
    ┌ CALL STACK ──────────────────────── 5 calls · 2 live ┐
    │      M.add "b" 2 M.empty               map_fold.ml:8 │
    │  ▾ M.add "a" 1 (M.add "b" 2 M.empty)   map_fold.ml:8 │
    │ ▎    M.add k (v * 2) acc              map_fold.ml:12 │
    │      M.add k (v * 2) acc              map_fold.ml:12 │
    │    M.fold (fun k v acc -> M.add k (v  map_fold.ml:10 │
    │      * 2) acc) m M.empty                             │
    │                                                      │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: a map's l edge is empty, its r edge walked" =
  let replay = replay_of_fixture "map_basic" in
  heap_view replay ~step:1;
  [%expect
    {|
    ┌ HEAP ────────────────────── 2 live · 3 nodes · 2 new ┐
    │ ▾ m · map                                            │
    │  ┌────────────────┐                                  │
    │  │ m · "a" ↦ 1    │                                  │
    │  │ 0x70f32cff19c0 │                                  │
    │  └────────────────┘                                  │
    │                                                      │
    │ ▾ m · map                                            │
    │ ▾┌─────────── new ┐                                  │
    │  │ m · "a" ↦ 1    │                                  │
    │  │ 0x70f32cfee710 │                                  │
    │  └────────────────┘                                  │
    │    ┌─────┴──────┐                                    │
    │    l            r                                    │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: a queue chains cells off first/next" =
  let replay = replay_of_fixture "queue_basic" in
  heap_view replay ~step:2;
  [%expect
    {|
    ┌ HEAP ────────────────────── 1 live · 3 nodes · 1 new ┐
    │ ▾ q · queue                                          │
    │ ▾┌────────────────┐                                  │
    │  │ q · length 2   │                                  │
    │  │ 0x7ec2e43f2200 │                                  │
    │  └────────────────┘                                  │
    │          │                                           │
    │        first                                         │
    │ ▾┌────────────────┐                                  │
    │  │ "x"            │                                  │
    │  │ 0x7ec2e43ef970 │                                  │
    │  └────────────────┘                                  │
    │          │                                           │
    │        next                                          │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: boxed map data becomes a d→ child" =
  let replay = replay_of_fixture "map_data_kinds" in
  heap_view replay ~step:2;
  [%expect
    {|
    ┌ HEAP ────────────────────── 3 live · 5 nodes · 2 new ┐
    │ ▾ m · map                                            │
    │  ┌─────────────────┐                                 │
    │  │ m · "pi" ↦ 3.14 │                                 │
    │  │ 0x702d533f19c0  │                                 │
    │  └─────────────────┘                                 │
    │                                                      │
    │ ▾ #2 · map                                           │
    │      ▾┌──────────────────┐                           │
    │       │ #2 · "pi" ↦ 3.14 │                           │
    │       │ 0x702d533ee4d0   │                           │
    │       └──────────────────┘                           │
    │          ┌─────┴──────┐                              │
    │          l            r                              │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: a collected structure is simply gone" =
  (* the case tracks a map that dies inside [make], forces a full major
     collection, then tracks another: before, the pane shows the doomed map;
     after, the registry no longer carries it and neither do we *)
  let replay = replay_of_fixture "map_registry_gc" in
  heap_view ~height:8 replay ~step:0;
  heap_view ~height:8 replay ~step:1;
  [%expect
    {|
    ┌ HEAP ────────────────────── 1 live · 1 nodes · 1 new ┐
    │ ▾ #1 · map                                           │
    │  ┌──────────── new ┐                                 │
    │  │ #1 · "dead" ↦ 0 │                                 │
    │  │ 0x7647edff0770  │                                 │
    │  └─────────────────┘                                 │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    ┌ HEAP ────────────────────── 1 live · 1 nodes · 1 new ┐
    │ ▾ #2 · map                                           │
    │  ┌──────────── new ┐                                 │
    │  │ #2 · "live" ↦ 1 │                                 │
    │  │ 0x7647edffffd8  │                                 │
    │  └─────────────────┘                                 │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "source pane: gutter, active line wash, callsite marker" =
  let source =
    Jsip_parsing.Source_reader.load "../../../testing/cases/map_basic.ml"
    |> Or_error.map ~f:Source_pane.Loaded.of_source_file
  in
  print_view
    ~height:11
    (Source_pane.view
       ~width:56
       ~height:11
       ~file_label:"map_basic.ml"
       ~source
       ~folds:Int.Set.empty
       ~active_line:8
       ~callsite_line:(Some 7)
       ~char_range:(10, 23));
  [%expect
    {|
    ┌ SOURCE ───────────────────── map_basic.ml · 10 lines ┐
    │            value) and [ignore] don't. *)             │
    │  ▾  3 module M = Map.Make (String)                   │
    │     4                                                │
    │  ▾  5 let () =                                       │
    │     6   let m = M.empty in                           │
    │ ▸   7   let m = M.add "a" 1 m in                     │
    │ ▎   8   let m = M.add "b" 2 m in                     │
    │     9   let m = M.remove "a" m in                    │
    │    10   ignore (M.find "b" m)                        │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "source pane: a missing file renders its error" =
  print_view
    ~height:5
    (Source_pane.view
       ~width:56
       ~height:5
       ~file_label:"gone.ml"
       ~source:(Or_error.error_string "no source loaded for gone.ml")
       ~folds:Int.Set.empty
       ~active_line:1
       ~callsite_line:None
       ~char_range:(0, 0));
  [%expect
    {|
    ┌ SOURCE ─────────────────────────── gone.ml · missing ┐
    │                                                      │
    │  no source loaded for gone.ml                        │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "transport: ticks, then the clickable key legend" =
  print_view
    ~height:3
    (Transport.view ~width:56 ~step:1 ~total:3 ~playing:false);
  [%expect
    {|
    ━━━━━━━━━━━━━━━━━ ━━━━━━━━━━━━━━━━━ ━━━━━━━━━━━━━━━━━
             ◂ back  ·  step ▸  ·  [space] play  ·  q quit
    |}]
;;

let%expect_test "syntax spans" =
  let spans, depth =
    Syntax.line ~comment_depth:0 "let m = M.add \"b\" 2 m (* nice *)"
  in
  print_s [%sexp (spans : (Syntax.Token.t * string) list)];
  print_s [%sexp (depth : int)];
  [%expect
    {|
    ((Keyword let) (Plain " ") (Plain m) (Plain " ") (Operator =) (Plain " ")
     (Uident M) (Operator .) (Plain add) (Plain " ") (String "\"b\"") (Plain " ")
     (Number 2) (Plain " ") (Plain m) (Plain " ") (Comment "(*")
     (Comment " nice *)"))
    0
    |}]
;;

let%expect_test "tick hit-testing round-trips" =
  let width = 26 in
  let total = 3 in
  let hits =
    List.init width ~f:(fun x -> Transport.step_at ~width ~total ~x)
    |> List.filter_map ~f:Fn.id
  in
  print_s [%sexp (List.dedup_and_sort hits ~compare : int list)];
  [%expect {| (0 1 2) |}]
;;

let%expect_test "wrap: words fold at the width, long words hard-split" =
  Wrap.spans [ `A, "alpha beta"; `B, " gamma_delta_epsilon zz" ] ~width:12
  |> List.iter ~f:(fun line ->
    List.map line ~f:(fun ((_ : [ `A | `B ]), text) -> text)
    |> String.concat
    |> fun text -> print_endline [%string "[%{text}]"]);
  [%expect {|
    [alpha beta ]
    [gamma_delta_]
    [epsilon zz]
    |}]
;;

let%expect_test "source pane: long lines wrap under a blank gutter" =
  let source =
    Jsip_parsing.Source_reader.load "../../../testing/cases/map_fold.ml"
    |> Or_error.map ~f:Source_pane.Loaded.of_source_file
  in
  print_view
    ~height:10
    (Source_pane.view
       ~width:44
       ~height:10
       ~file_label:"map_fold.ml"
       ~source
       ~folds:Int.Set.empty
       ~active_line:9
       ~callsite_line:None
       ~char_range:(16, 60));
  [%expect
    {|
    ┌ SOURCE ────────── map_fold.ml · 15 lines ┐
    │     6                                    │
    │  ▾  7 let () =                           │
    │     8   let m = M.add "a" 1 (M.add "b"   │
    │           2 M.empty) in                  │
    │ ▎   9   let doubled =                    │
    │    10     M.fold                         │
    │    11       (fun k v acc ->              │
    │    12         M.add k (v * 2) acc)       │
    └──────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: a union's two subtrees share a level" =
  let replay = replay_of_fixture "set_ops" in
  heap_view ~width:60 replay ~step:2;
  [%expect
    {|
    ┌ HEAP ────────────────────────── 3 live · 9 nodes · 4 new ┐
    │ ▾ a · set                                                │
    │ ▾┌────────────────┐                                      │
    │  │ a · 1          │                                      │
    │  │ 0x7c08425f17c0 │                                      │
    │  └────────────────┘                                      │
    │    ┌─────┴──────┐                                        │
    │    l            r                                        │
    │    ∅   ▾┌────────────────┐                               │
    │         │ 2              │                               │
    │         │ 0x7c08425f17e8 │                               │
    │         └────────────────┘                               │
    │           ┌─────┴──────┐                                 │
    │           l            r                                 │
    └──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap clicks land on cards, not the space between" =
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  let at ~x ~y =
    Heap_pane.address_at
      ~structures
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~scroll:0
      ~height:15
      ~x
      ~y
    |> Option.value_map ~default:"·" ~f:Snapshot.Address.display
  in
  (* the root card, the child card, and the gap beside the rail *)
  print_endline (at ~x:2 ~y:1);
  print_endline (at ~x:10 ~y:8);
  print_endline (at ~x:30 ~y:5);
  [%expect {|
    0x70f32cff19c0
    0x70f32cfee710
    ·
    |}]
;;

let%expect_test "heap pane: the map outlives the queue's arrival" =
  (* queue_of_maps: after [Queue.create] the registry holds both the map (id
     1, its earlier walk) and the fresh queue (id 2, current) *)
  let replay = replay_of_fixture "queue_of_maps" in
  heap_view ~height:14 replay ~step:1;
  [%expect
    {|
    ┌ HEAP ────────────────────── 2 live · 2 nodes · 1 new ┐
    │ ▾ m · map                                            │
    │  ┌────────────────┐                                  │
    │  │ m · "k" ↦ 1    │                                  │
    │  │ 0x7be786ff16b0 │                                  │
    │  └────────────────┘                                  │
    │                                                      │
    │ ▾ q · queue                                          │
    │  ┌─────────── new ┐                                  │
    │  │ q · length 0   │                                  │
    │  │ 0x7be786fee428 │                                  │
    │  └────────────────┘                                  │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: Queue.add links the map into the queue's tree" =
  (* the cell's content is (Id 1) — the registry resolves it, so the map's
     tree hangs off the cell's v→ edge, tagged #1 *)
  let replay = replay_of_fixture "queue_of_maps" in
  heap_view ~height:21 replay ~step:2;
  [%expect
    {|
    ┌ HEAP ────────────────────── 2 live · 3 nodes · 1 new ┐
    │ ▾ q · queue                                          │
    │       ▾┌────────────────┐                            │
    │        │ q · length 1   │                            │
    │        │ 0x7be786fee428 │                            │
    │        └────────────────┘                            │
    │                │                                     │
    │              first                                   │
    │       ▾┌─────────── new ┐                            │
    │        │ ·              │                            │
    │        │ 0x7be786feb7b0 │                            │
    │        └────────────────┘                            │
    │          ┌─────┴──────┐                              │
    │          v          next                             │
    │  ┌────────────────┐   ∅                              │
    │  │ m · "k" ↦ 1    │                                  │
    │  │ 0x7be786ff16b0 │                                  │
    │  └────────────────┘                                  │
    │                                                      │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: hashtbl walks record → bucket array → chain" =
  let replay = replay_of_fixture "hashtbl_basic" in
  heap_view ~width:60 ~height:17 replay ~step:1;
  [%expect
    {|
    ┌ HEAP ────────────────────────── 1 live · 3 nodes · 1 new ┐
    │ ▾ tbl · hashtbl                                          │
    │ ▾┌────────────────┐                                      │
    │  │ tbl · size 1   │                                      │
    │  │ 0x7f4241feee40 │                                      │
    │  └────────────────┘                                      │
    │          │                                               │
    │        data                                              │
    │ ▾┌────────────────┐                                      │
    │  │ slots 16       │                                      │
    │  │ 0x7f4241feee68 │                                      │
    │  └────────────────┘                                      │
    │          │                                               │
    │          1                                               │
    │  ┌─────────── new ┐                                      │
    │  │ "a" ↦ 1        │                                      │
    └──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: Queue.transfer observes both roots in one frame" =
  let replay = replay_of_fixture "queue_transfer" in
  let show step =
    let { Replay.Step.call; structures; _ } = Replay.step_exn replay ~step in
    let names =
      List.map structures ~f:(fun structure ->
        let mark =
          match structure.is_current with true -> "▸" | false -> " "
        in
        [%string "%{mark}%{Replay.Structure.display structure}"])
      |> String.concat ~sep:"  "
    in
    print_endline
      [%string
        "%{step#Int}: %{Function_info.display call.info.function_info} — \
         %{names}"]
  in
  List.iter (List.init (Replay.length replay) ~f:Fn.id) ~f:show;
  [%expect
    {|
    0: Queue.create — ▸q1
    1: Queue.create —  q1  ▸q2
    2: Queue.add — ▸q1   q2
    3: Queue.add — ▸q1   q2
    4: Queue.transfer — ▸q1   q2
    5: Queue.transfer —  q1  ▸q2
    |}]
;;

let%expect_test "heap pane: a queue of queues links through Id boundaries" =
  (* after both adds, [qq]'s cells hold (Id 2) and (Id 3) — the inner queues
     draw inside qq's tree; the later pops give them back their own sections *)
  let replay = replay_of_fixture "queue_of_queues" in
  heap_view ~width:60 ~height:26 replay ~step:3;
  [%expect
    {|
    ┌ HEAP ────────────────────────── 2 live · 3 nodes · 1 new ┐
    │ ▾ qq · queue                                             │
    │       ▾┌────────────────┐                                │
    │        │ qq · length 1  │                                │
    │        │ 0x7031011f2200 │                                │
    │        └────────────────┘                                │
    │                │                                         │
    │              first                                       │
    │       ▾┌─────────── new ┐                                │
    │        │ ·              │                                │
    │        │ 0x7031011eccd0 │                                │
    │        └────────────────┘                                │
    │          ┌─────┴──────┐                                  │
    │          v          next                                 │
    │  ┌────────────────┐   ∅                                  │
    │  │ q1 · length 0  │                                      │
    │  │ 0x7031011ef958 │                                      │
    │  └────────────────┘                                      │
    │                                                          │
    │                                                          │
    │                                                          │
    │                                                          │
    │                                                          │
    │                                                          │
    │                                                          │
    └──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: closures stay opaque" =
  let replay = replay_of_fixture "queue_of_closures" in
  heap_view ~width:60 ~height:13 replay ~step:1;
  [%expect
    {|
    ┌ HEAP ────────────────────────── 1 live · 2 nodes · 2 new ┐
    │ ▾ q · queue                                              │
    │  ▾┌────────────────┐                                     │
    │   │ q · length 1   │                                     │
    │   │ 0x724d0fdf21f8 │                                     │
    │   └────────────────┘                                     │
    │           │                                              │
    │         first                                            │
    │  ┌───────────── new ┐                                    │
    │  │ ⟨0x724d0fdef958⟩ │                                    │
    │  │ 0x724d0fdef940   │                                    │
    │  └──────────────────┘                                    │
    └──────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "control chips hit-test exactly where they render" =
  let width = 56 in
  let hits =
    List.filter_map (List.init width ~f:Fn.id) ~f:(fun x ->
      Transport.control_at ~width ~playing:false ~x
      |> Option.map ~f:(fun button -> x, button))
  in
  let groups =
    List.group hits ~break:(fun ((_ : int), a) ((_ : int), b) ->
      not (Transport.Button.equal a b))
  in
  List.iter groups ~f:(fun group ->
    let first, button = List.hd_exn group in
    let last, (_ : Transport.Button.t) = List.last_exn group in
    print_s
      [%sexp (button : Transport.Button.t), (first : int), (last : int)]);
  [%expect
    {|
    (Back 10 15)
    (Step 21 26)
    (Play 32 43)
    (Quit 49 54)
    |}]
;;

let%expect_test "heap fold: a card keeps itself, hides its kids" =
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  print_view
    ~height:10
    (Heap_pane.view
       ~width:56
       ~height:10
       ~structures
       ~new_addresses
       ~folds:
         (Set.of_list
            (module Heap_pane.Fold)
            [ Heap_pane.Fold.Node (2, []) ])
       ~scroll:0);
  [%expect
    {|
    ┌ HEAP ────────────────────── 2 live · 3 nodes · 2 new ┐
    │ ▾ m · map                                            │
    │  ┌────────────────┐                                  │
    │  │ m · "a" ↦ 1    │                                  │
    │  │ 0x70f32cff19c0 │                                  │
    │  └────────────────┘                                  │
    │                                                      │
    │ ▾ m · map                                            │
    │ ▸┌─ new  ⋯ 1 hidden ┐                                │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap fold: a structure collapses to its header" =
  let replay = replay_of_fixture "queue_of_maps" in
  let { Replay.Step.structures; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  (* folding the queue also keeps the map it references hidden — folded means
     folded, not spilled back out as a section *)
  print_view
    ~height:8
    (Heap_pane.view
       ~width:56
       ~height:8
       ~structures
       ~new_addresses
       ~folds:
         (Set.of_list (module Heap_pane.Fold) [ Heap_pane.Fold.Structure 2 ])
       ~scroll:0);
  [%expect
    {|
    ┌ HEAP ────────────────────── 2 live · 3 nodes · 1 new ┐
    │ ▸ q · queue                                          │
    │                                                      │
    │                                                      │
    │                                                      │
    │                                                      │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap fold: toggles sit where the glyphs render" =
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  let toggle ~x ~y =
    Heap_pane.toggle_at
      ~structures
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~scroll:0
      ~height:12
      ~x
      ~y
    |> Option.value_map ~default:"·" ~f:(fun fold ->
      Sexp.to_string [%sexp (fold : Heap_pane.Fold.t)])
  in
  (* #1's header glyph; #1's card is a leaf (no glyph); #2's header and
     root-card glyphs; a plain card cell *)
  print_endline (toggle ~x:0 ~y:0);
  print_endline (toggle ~x:0 ~y:1);
  print_endline (toggle ~x:0 ~y:6);
  print_endline (toggle ~x:0 ~y:7);
  print_endline (toggle ~x:3 ~y:8);
  [%expect
    {|
    (Structure 1)
    ·
    (Structure 2)
    (Node 2())
    ·
    |}]
;;

let%expect_test "stack fold: a call's range tucks behind a count" =
  let replay = replay_of_fixture "map_fold" in
  print_view
    ~height:7
    (Stack_pane.view
       ~width:56
       ~height:7
       ~calls:(calls_of replay)
       ~live:(live_of replay ~step:4)
       ~selected:0
       ~folds:(Int.Set.of_list [ 1 ]));
  [%expect
    {|
    ┌ CALL STACK ──────────────────────── 5 calls · 1 live ┐
    │      M.add "b" 2 M.empty               map_fold.ml:8 │
    │  ▸ M.add "a" 1 (M.add "b" 2 M.empty)   map_fold.ml:8 │
    │      ⋯ 2                                             │
    │ ▎  M.fold (fun k v acc -> M.add k (v  map_fold.ml:10 │
    │ ▎    * 2) acc) m M.empty                             │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "source fold: a definition folds to its first line" =
  let source =
    Jsip_parsing.Source_reader.load "../../../testing/cases/map_basic.ml"
    |> Or_error.map ~f:Source_pane.Loaded.of_source_file
  in
  print_view
    ~height:8
    (Source_pane.view
       ~width:56
       ~height:8
       ~file_label:"map_basic.ml"
       ~source
       ~folds:(Int.Set.of_list [ 5 ])
       ~active_line:8
       ~callsite_line:None
       ~char_range:(10, 23));
  [%expect
    {|
    ┌ SOURCE ───────────────────── map_basic.ml · 10 lines ┐
    │         add/add/remove fire (3 events);              │
    │     2    [empty] (an ident), [find] (returns the     │
    │            value) and [ignore] don't. *)             │
    │  ▾  3 module M = Map.Make (String)                   │
    │     4                                                │
    │ ▎▸  5 let () = ⋯ 5 lines                             │
    └──────────────────────────────────────────────────────┘
    |}]
;;
