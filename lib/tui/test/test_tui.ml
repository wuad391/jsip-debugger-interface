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

let heap_view ?(width = 56) ?(height = 9) replay ~step =
  let { Replay.Step.call; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  print_view
    ~width
    ~height
    (Heap_pane.view
       ~width
       ~height
       ~snapshot:call.info.snapshot
       ~registry:call.info.registry
       ~new_addresses
       ~scroll:0)
;;

let%expect_test "stack pane: map_fold's callback runs inside the fold" =
  let replay = replay_of_fixture "map_fold" in
  let { Replay.Step.frames; _ } = Replay.step_exn replay ~step:2 in
  print_view
    ~height:5
    (Stack_pane.view ~width:56 ~height:5 ~frames ~selected:1);
  [%expect
    {|
    ┌ CALL STACK ────────────────────────────────── 2 live ┐
    │  M.add "a" 1 (M.add "b" 2 M.empty)     map_fold.ml:8 │
    │ ▎  M.add k (v * 2) acc                map_fold.ml:12 │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: a map's l edge is empty, its r edge walked" =
  let replay = replay_of_fixture "map_basic" in
  heap_view replay ~step:1;
  [%expect
    {|
    ┌ HEAP ───────────────────────── map · 2 nodes · 2 new ┐
    │ live  1↦0x763be65f19e8  2↦0x763be65ee878             │
    │                                                      │
    │ ● 0x763be65ee878  v="a"  d=1  new                    │
    │ ├─l→ ∅                                               │
    │ └─r→ ● 0x763be65ee8a8  v="b"  d=2  new               │
    │      ├─l→ ∅                                          │
    │      └─r→ ∅                                          │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: a queue chains cells off first/next" =
  let replay = replay_of_fixture "queue_basic" in
  heap_view replay ~step:2;
  [%expect
    {|
    ┌ HEAP ─────────────────────── queue · 3 nodes · 1 new ┐
    │ live  1↦0x70a6aa9f2228                               │
    │                                                      │
    │ ● 0x70a6aa9f2228  length=2                           │
    │ └─first→ ● 0x70a6aa9efb50  v="x"                     │
    │          └─next→ ● 0x70a6aa9ec9e0  v="y"  new        │
    │                  └─next→ ∅                           │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: boxed map data becomes a d→ child" =
  let replay = replay_of_fixture "map_data_kinds" in
  heap_view replay ~step:2;
  [%expect
    {|
    ┌ HEAP ───────────────────────── map · 2 nodes · 2 new ┐
    │ live  1↦0x7cc39e1f19e8  2↦0x7cc39e1ee630  3↦0x7cc39e │
    │                                                      │
    │ ● 0x7cc39e1e9c50  v="pair"  new                      │
    │ ├─l→ ∅                                               │
    │ ├─d→ ● 0x7cc3ae369b18  0=1  1="one"  new             │
    │ └─r→ ∅                                               │
    │                                                      │
    └──────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: the registry strip drops GC'd structures" =
  let replay = replay_of_fixture "map_registry_gc" in
  heap_view ~height:6 replay ~step:1;
  [%expect
    {|
    ┌ HEAP ───────────────────────── map · 1 nodes · 1 new ┐
    │ live  2↦0x7647edffffd8                               │
    │                                                      │
    │ ● 0x7647edffffd8  v="live"  d=1  new                 │
    │ ├─l→ ∅                                               │
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
       ~active_line:8
       ~callsite_line:(Some 7)
       ~char_range:(10, 23));
  [%expect
    {|
    ┌ SOURCE ───────────────────── map_basic.ml · 11 lines ┐
    │    3 module M = Map.Make (String)                    │
    │    4                                                 │
    │    5 let () =                                        │
    │    6   let m = M.empty in                            │
    │ ▸  7   let m = M.add "a" 1 m in                      │
    │ ▎  8   let m = M.add "b" 2 m in                      │
    │    9   let m = M.remove "a" m in                     │
    │   10   ignore (M.find "b" m)                         │
    │   11 ;;                                              │
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

let%expect_test "footer: ticks mark past, current, future" =
  print_view
    ~height:3
    (Footer.view
       ~width:56
       ~step:1
       ~total:3
       ~playing:false
       ~status:"M.add \"b\" 2 m — map_basic.ml:8");
  [%expect
    {|
    ────────────────────────────────────────────────────────
     ━━━━━━━━━━━━━━━━━ ━━━━━━━━━━━━━━━━━ ━━━━━━━━━━━━━━━━━
     ◂ back  step ▸  ⏵ play  │ ▎ M.add "b" 2 m — map_basic.m
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
    List.init width ~f:(fun x -> Footer.step_at ~width ~total ~x)
    |> List.filter_map ~f:Fn.id
  in
  print_s [%sexp (List.dedup_and_sort hits ~compare : int list)];
  [%expect {| (0 1 2) |}]
;;
