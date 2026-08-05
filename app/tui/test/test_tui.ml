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
  let parsed_info =
    Dump_reader.read [%string "../../../testing/expected/%{name}.dump"]
    |> Or_error.ok_exn
  in
  Replay.create (Call_stack.create ~parsed_info)
;;

(* the pane the way it draws with nothing chosen and nothing aimed at: no
   card spells out its address, which is the common case on screen *)
let heap_view
  ?(width = 56)
  ?(height = 15)
  ?(scroll = 0)
  ?(pan = 0)
  ?(selection = Heap_pane.Selection.none)
  ?folds
  replay
  ~step
  =
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  print_view
    ~width
    ~height
    (Heap_pane.view
       ~note:None
       ~total:None
       ~width
       ~height
       ~structures
       ~nodes
       ~new_addresses
       ~folds:
         (Option.value folds ~default:(Set.empty (module Heap_pane.Fold)))
       ~scroll
       ~pan
       ~selection)
;;

(* the structure this step walked — what the app selects by default, and so
   the card that shows its address *)
let current_spot replay ~step =
  List.find
    (Replay.step_exn replay ~step).structures
    ~f:(fun (structure : Replay.Structure.t) -> structure.is_current)
  |> Option.map ~f:Heap_pane.spot_of_structure
;;

let calls_of replay =
  Array.init (Replay.length replay) ~f:(fun step ->
    (Replay.step_exn replay ~step).call)
;;

(* no profile loaded: every callee keeps its ordinary state color *)
let no_heat calls = Array.map calls ~f:(fun (_ : Call.t) -> None)

let live_of replay ~step =
  let { Replay.Step.frames; _ } = Replay.step_exn replay ~step in
  List.map frames ~f:(fun (frame : Call.t) -> snd frame.range)
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
       ~heat:(no_heat (calls_of replay))
       ~live:(live_of replay ~step:2)
       ~selected:1
       ~folds:Int.Set.empty
       ~cursor:None
       ~collapsed:false);
  [%expect
    {|
    ▾ CALL STACK                          5 calls · 2 live
         M.add "b" 2 M.empty
     ▾ M.add "a" 1 (M.add "b" 2 M.empty)
    ▎    M.add k (v * 2) acc
         M.add k (v * 2) acc
     ▾ M.fold (fun k v acc -> M.add k (v * 2) acc) m
         M.empty
    |}]
;;

let%expect_test "heap pane: a map's l edge is empty, its r edge walked" =
  let replay = replay_of_fixture "map_basic" in
  heap_view replay ~step:1;
  [%expect
    {|
    HEAP                  2 live · 3 nodes · 168 B · 2 new
    ▾ m · map ⟨string ⇒ int⟩ · 2 nodes · 112 B
    ▾┌ m  new ┐
     │"a" → 1 │
     └────────┘
      ┌──────┴───────┐
      l              r
    ┌┄┄┄┐    ┌── new ┐
    ┆ ∅ ┆    │"b" → 2│
    └┄┄┄┘    └───────┘

    ▾ m · map ⟨string ⇒ int⟩ · 1 node · 56 B
     ┌ m ────┐
     │"a" → 1│
     └───────┘
    |}]
;;

let%expect_test "heap pane: a queue chains cells off first/next" =
  let replay = replay_of_fixture "queue_basic" in
  heap_view replay ~step:2;
  [%expect
    {|
    HEAP                  1 live · 3 nodes · 104 B · 1 new
    ▾ q · queue ⟨string⟩ · 3 nodes · 104 B
    ▾┌ q ─────┐
     │length 2│
     └────────┘
             │
           first
    ▾┌───┐
     │"x"│
     └───┘
             │
             1
     ┌ new ┐
     │"y"  │
     └─────┘
    |}]
;;

let%expect_test "heap pane: boxed map data becomes a d→ child" =
  let replay = replay_of_fixture "map_data_kinds" in
  heap_view replay ~step:2;
  [%expect
    {|
    HEAP                  3 live · 5 nodes · 312 B · 2 new
    ▾ #4 · map ⟨string ⇒ int * string⟩ · 2 nodes · 96 B
            ▾┌ #4  new ┐
             │"pair"   │
             └─────────┘
      ┌──────────────┼──────────────┐
      l              d              r
    ┌┄┄┄┐    ┌─── new ┐           ┌┄┄┄┐
    ┆ ∅ ┆    │1, "one"│           ┆ ∅ ┆
    └┄┄┄┘    └────────┘           └┄┄┄┘

    ▾ #2 · map ⟨string ⇒ float⟩ · 2 nodes · 144 B
           ▾┌ #2 ───────┐
            │"pi" → 3.14│
            └───────────┘
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
    HEAP                    1 live · 1 node · 56 B · 1 new
    ▾ #1 · map ⟨string ⇒ int⟩ · 1 node · 56 B
     ┌ #1 ─ new ┐
     │"dead" → 0│
     └──────────┘
    HEAP                    1 live · 1 node · 56 B · 1 new
    ▾ #2 · map ⟨string ⇒ int⟩ · 1 node · 56 B
     ┌ #2 ─ new ┐
     │"live" → 1│
     └──────────┘
    |}]
;;

let%expect_test "heap pane: a Core map is a record over a tagged tree" =
  (* Core reaches its maps through Base, whose map is a record holding the
     comparator and the tree — not the tree itself, the way the stdlib's is.
     The pane reads that straight off the wire: [tree] is the root's one
     edge, and the nodes below it are Base's own [Leaf]/[Node] shapes. *)
  let replay = replay_of_fixture "core_map_basic" in
  heap_view ~width:60 ~height:12 replay ~step:2;
  [%expect
    {|
    HEAP                      3 live · 8 nodes · 280 B · 3 new
    ▾ m · core.map ⟨string ⇒ int⟩ · 3 nodes · 112 B
               ▾┌ m  new ┐
                │·       │
                └────────┘
                        │
                      tree
               ▾┌── new ┐
                │"b" → 2│
                └───────┘
             ┌──────────┴──────────┐
             l                     r
    |}]
;;

let%expect_test "heap pane: a Core hash queue chains through its elements" =
  (* the order-book shape: a doubly-linked list of key/data pairs, where an
     element's [next] stays on its own layer while [v] steps down to the
     pair. Nothing here needs a layout — every edge arrives labeled. *)
  let replay = replay_of_fixture "core_hash_queue" in
  heap_view ~width:64 ~height:22 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                          1 live · 9 nodes · 240 B · 2 new
    ▾ q · core.hash_queue ⟨string ⇒ int⟩ · 9 nodes · 240 B
                       ▾┌ q ┐
                        │·  │
                        └───┘
                                │
                              queue
                       ▾┌─┐
                        │·│
                        └─┘
                                │
                            contents
                       ▾┌─┐
                        │·│
                        └─┘
                                │
                                0
                       ▾┌─┐
                        │·│
                        └─┘
             ┌──────────────────┴──────────────────┐
             v                                   next
    |}]
;;

let%expect_test "heap pane: a user type is drawn from its derived schema" =
  (* [User] has no skeleton of its own: the walker labels its fields from the
     schema the instrumentation derived, so a list cell reads [hd]/[tl] and
     the pane prints exactly that. *)
  let replay = replay_of_fixture "user_types" in
  heap_view ~width:60 ~height:14 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                      3 live · 5 nodes · 160 B · 1 new
    ▾ ts · user ⟨trades⟩ · 1 node · 24 B
               ▾┌ ts  new ┐
                │0        │
                └─────────┘
                        │
                       hd
               ▾┌ t ┐
                │101│
                └───┘
             ┌──────────┴──────────┐
           tags                  span
     ┌──────────────┐      ┌────┐
     │"buy", "limit"│      │1, 9│
    |}]
;;

let%expect_test "multi-file: each live frame knows its own file" =
  (* the dump spans three modules, and mid-fold the live chain crosses two of
     them — [Queue.fold] still open in main.ml while [restock]'s add fires in
     inventory.ml (steps 5 and 6). That per-frame file is what lets the
     source pane swap files as the blue selection moves up the call stack.
     Ancestors complete after their children, so the outer frame's own event
     lies later in the dump than the inner's. *)
  let replay = replay_of_fixture "multi_file" in
  print_s [%sexp (Replay.files replay : string list)];
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.frames; _ } = Replay.step_exn replay ~step in
      let chain =
        List.map frames ~f:(fun (frame : Call.t) ->
          let fn = Function_info.display frame.info.function_info in
          let file =
            Filename.basename (Location.file_path frame.info.location)
          in
          [%string "%{fn}@%{file}"])
        |> String.concat ~sep:" > "
      in
      print_endline [%string "%{step#Int}: %{chain}"]);
  [%expect
    {|
    (testing/cases/multi_file/inventory.ml testing/cases/multi_file/basket.ml
     testing/cases/multi_file/main.ml)
    0: M.add@inventory.ml > M.add@inventory.ml
    1: M.add@inventory.ml
    2: Queue.create@basket.ml
    3: Queue.add@basket.ml
    4: Queue.add@basket.ml
    5: Queue.fold@main.ml > M.add@inventory.ml
    6: Queue.fold@main.ml > M.add@inventory.ml
    7: Queue.fold@main.ml
    8: Queue.fold@main.ml
    |}]
;;

let%expect_test "the left panes collapse to their title rows" =
  (* [1] /[2] (or a click on the title) fold a pane to exactly its title —
         the [▸] and the counts stay, and the layout hands the freed height
         to the pane underneath (or above) *)
  let replay = replay_of_fixture "map_fold" in
  print_view
    ~width:56
    ~height:2
    (Stack_pane.view
       ~width:56
       ~height:1
       ~calls:(calls_of replay)
       ~heat:(no_heat (calls_of replay))
       ~live:(live_of replay ~step:2)
       ~selected:1
       ~folds:Int.Set.empty
       ~cursor:None
       ~collapsed:true);
  let heights ?stack_collapsed ?source_collapsed () =
    let layout =
      Layout.compute
        ?stack_collapsed
        ?source_collapsed
        { Bonsai_term.Dimensions.width = 100; height = 40 }
    in
    layout.stack.height, layout.source.height
  in
  print_s
    [%message
      ""
        ~both_open:(heights () : int * int)
        ~stack_shut:(heights ~stack_collapsed:true () : int * int)
        ~source_shut:(heights ~source_collapsed:true () : int * int)];
  [%expect
    {|
     ▸ CALL STACK                          5 calls · 2 live
    ((both_open (19 15)) (stack_shut (1 33)) (source_shut (33 1)))
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
       ~char_range:(10, 23)
       ~collapsed:false);
  [%expect
    {|
    ▾ SOURCE                       map_basic.ml · 10 lines
        2    [empty] (an ident), [find] (returns the
               value) and [ignore] don't. *)
     ▾  3 module M = Map.Make (String)
        4
     ▾  5 let () =
        6   let m = M.empty in
    ▸   7   let m = M.add "a" 1 m in
    ▎   8   let m = M.add "b" 2 m in
        9   let m = M.remove "a" m in
       10   ignore (M.find "b" m)
    |}]
;;

let%expect_test "source pane: a missing file renders its error, wrapped" =
  (* the placeholder the app builds names the resolved path and the flag that
     moves the search — a sentence, so it has to wrap in a pane a third of
     the screen wide rather than crop *)
  print_view
    ~width:36
    ~height:8
    (Source_pane.view
       ~width:36
       ~height:8
       ~file_label:"gone.ml"
       ~source:
         (Or_error.error_string
            "lib/gone.ml is not at ./lib/gone.ml — the dump's paths resolve \
             from the replayed program's root, so run there or pass \
             -source-root DIR")
       ~folds:Int.Set.empty
       ~active_line:1
       ~callsite_line:None
       ~char_range:(0, 0)
       ~collapsed:false);
  [%expect
    {|
    ▾ SOURCE         gone.ml · missing

     lib/gone.ml is not at
     ./lib/gone.ml — the dump's
     paths resolve from the replayed
     program's root, so run there or
     pass -source-root DIR
    |}]
;;

let%expect_test "transport: ticks, then the clickable key legend" =
  print_view
    ~width:84
    ~height:3
    (Transport.view
       ~width:84
       ~step:1
       ~total:3
       ~playing:false
       ~accordion:false);
  [%expect
    {|
    ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    ◂ back · step ▸ · [space] play · . latest · h fold · z accordion · / filter · q quit
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
       ~char_range:(16, 60)
       ~collapsed:false);
  [%expect
    {|
    ▾ SOURCE            map_fold.ml · 15 lines
        6
     ▾  7 let () =
        8   let m = M.add "a" 1 (M.add "b" 2
              M.empty) in
    ▎   9   let doubled =
       10     M.fold
       11       (fun k v acc ->
       12         M.add k (v * 2) acc)
       13       m M.empty
    |}]
;;

let%expect_test "heap pane: a union's two subtrees share a level" =
  let replay = replay_of_fixture "set_ops" in
  heap_view ~width:60 replay ~step:2;
  [%expect
    {|
    HEAP                      3 live · 9 nodes · 288 B · 4 new
    ▾ #6 · set ⟨int⟩ · 4 nodes · 128 B
                      ▾┌ #6  new ┐
                       │3        │
                       └─────────┘
                    ┌──────────┴───────────┐
                    l                      r
           ▾┌ new ┐                ┌ new ┐
            │2    │                │4    │
            └─────┘                └─────┘
             ┌──────┴───────┐
             l              r
     ┌ new ┐              ┌┄┄┄┐
     │1    │              ┆ ∅ ┆
     └─────┘              └┄┄┄┘
    |}]
;;

let%expect_test "heap clicks land on cards, not the space between" =
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  let at ~x ~y =
    Heap_pane.spot_at
      ~width:56
      ~structures
      ~nodes
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~scroll:0
      ~pan:0
      ~selection:Heap_pane.Selection.none
      ~height:15
      ~x
      ~y
    |> Option.value_map ~default:"·" ~f:(fun (spot : Heap_pane.Spot.t) ->
      Snapshot.Address.display spot.address)
  in
  (* the root card, the second structure's header — a place of its own now —
     and the empty canvas right of the cards *)
  print_endline (at ~x:2 ~y:1);
  print_endline (at ~x:10 ~y:8);
  print_endline (at ~x:30 ~y:5);
  print_endline (at ~x:40 ~y:1);
  [%expect {|
    0x7fa801fee750
    0x7fa801fee780
    ·
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
    HEAP                   2 live · 2 nodes · 80 B · 1 new
    ▾ q · queue ⟨int M.t⟩ · 1 node · 24 B
     ┌ q  new ┐
     │length 0│
     └────────┘

    ▾ m · map ⟨string ⇒ int⟩ · 1 node · 56 B
     ┌ m ────┐
     │"k" → 1│
     └───────┘
    |}]
;;

let%expect_test "heap pane: Queue.add links the map into the queue's tree" =
  (* the cell's content is (Id 1) — the registry resolves it, so the map's
     tree hangs off the cell's v→ edge, tagged #1 *)
  let replay = replay_of_fixture "queue_of_maps" in
  heap_view ~height:21 replay ~step:2;
  [%expect
    {|
    HEAP                  2 live · 3 nodes · 104 B · 1 new
    ▾ q · queue ⟨int M.t⟩ · 2 nodes · 48 B
    ▾┌ q ─────┐
     │length 1│
     └────────┘
             │
           first
    ▾┌── new ┐
     │slots 2│
     └───────┘
             │
             0
     ┌ m ────┐
     │"k" → 1│
     └───────┘
    |}]
;;

let%expect_test "heap pane: hashtbl walks record → bucket array → chain" =
  let replay = replay_of_fixture "hashtbl_basic" in
  heap_view ~width:60 ~height:17 replay ~step:1;
  [%expect
    {|
    HEAP                      1 live · 3 nodes · 208 B · 1 new
    ▾ tbl · hashtbl ⟨string ⇒ int⟩ · 3 nodes · 208 B
    ▾┌ tbl ─┐
     │size 1│
     └──────┘
             │
           data
    ▾┌────────┐
     │slots 16│
     └────────┘
             │
             1
     ┌── new ┐
     │"a" → 1│
     └───────┘
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
    HEAP                       2 live · 3 nodes · 72 B · 1 new
    ▾ qq · queue ⟨'a Queue.t⟩ · 2 nodes · 48 B
    ▾┌ qq ────┐
     │length 1│
     └────────┘
             │
           first
    ▾┌── new ┐
     │slots 2│
     └───────┘
             │
             0
     ┌ q1 ────┐
     │length 0│
     └────────┘
    |}]
;;

let%expect_test "heap pane: closures stay opaque" =
  let replay = replay_of_fixture "queue_of_closures" in
  heap_view ~width:60 ~height:13 replay ~step:1;
  [%expect
    {|
    HEAP                       1 live · 2 nodes · 48 B · 2 new
    ▾ q · queue ⟨int -> int⟩ · 2 nodes · 48 B
    ▾┌ q ─────┐
     │length 1│
     └────────┘
             │
           first
     ┌─────────── new ┐
     │⟨0x73a5227efa18⟩│
     └────────────────┘
    |}]
;;

let%expect_test "control chips hit-test exactly where they render" =
  let width = 84 in
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
    (Back 0 5)
    (Step 9 14)
    (Play 18 29)
    (Latest 33 40)
    (Fold 44 49)
    (Accordion 53 63)
    (Filter 67 74)
    (Quit 78 83)
    |}]
;;

let%expect_test "heap fold: a card keeps itself, hides its kids" =
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  print_view
    ~height:10
    (Heap_pane.view
       ~note:None
       ~total:None
       ~width:56
       ~height:10
       ~structures
       ~nodes
       ~new_addresses
       ~folds:
         (Set.of_list
            (module Heap_pane.Fold)
            [ Heap_pane.Fold.Node (2, []) ])
       ~scroll:0
       ~pan:0
       ~selection:Heap_pane.Selection.none);
  [%expect
    {|
    HEAP                  2 live · 3 nodes · 168 B · 2 new
    ▾ m · map ⟨string ⇒ int⟩ · 2 nodes · 112 B
    ▸┌ m  new ┐
     │"a" → 1 │
     └────────┘
     ⋯ 1 hidden

    ▾ m · map ⟨string ⇒ int⟩ · 1 node · 56 B
     ┌ m ────┐
     │"a" → 1│
    |}]
;;

let%expect_test "heap fold: a structure collapses to its header" =
  let replay = replay_of_fixture "queue_of_maps" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  (* folding the queue also keeps the map it references hidden — folded means
     folded, not spilled back out as a section *)
  print_view
    ~height:8
    (Heap_pane.view
       ~note:None
       ~total:None
       ~width:56
       ~height:8
       ~structures
       ~nodes
       ~new_addresses
       ~folds:
         (Set.of_list (module Heap_pane.Fold) [ Heap_pane.Fold.Structure 2 ])
       ~scroll:0
       ~pan:0
       ~selection:Heap_pane.Selection.none);
  [%expect
    {|
    HEAP                  2 live · 3 nodes · 104 B · 1 new
    ▸ q · queue ⟨int M.t⟩ · 2 nodes · 48 B
    |}]
;;

(* render the heap body as lines, for comparing two states cell by cell *)
let heap_lines
  ?(width = 64)
  ?(height = 20)
  ?(folds = None)
  ~selection
  replay
  ~step
  =
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let folds =
    Option.value folds ~default:(Set.empty (module Heap_pane.Fold))
  in
  Bonsai_term.View.Private.notty_image
    (Heap_pane.view
       ~note:None
       ~total:None
       ~width
       ~height
       ~structures
       ~nodes
       ~new_addresses
       ~folds
       ~scroll:0
       ~pan:0
       ~selection)
  |> fun image ->
  let buffer = Buffer.create 1024 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (width, height) image;
  Buffer.contents buffer |> String.split_lines |> List.map ~f:String.rstrip
;;

let%expect_test "aiming moves the card, not the diagram" =
  (* The address rides the bottom border rather than taking a row, and every
     card is spaced as though it were showing one — so picking a card widens
     that card into room already set aside for it. Nothing else may move a
     cell, which is what these line numbers are: only the three rows of the
     card being aimed at differ. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let at_rest =
    heap_lines ~selection:Heap_pane.Selection.none replay ~step
  in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let aim cursor =
    Heap_pane.move_cursor
      ~structures
      ~nodes
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~selection:{ Heap_pane.Selection.selected = None; cursor }
      ~direction:Down
  in
  let root = aim None in
  let child = aim root in
  List.iter
    [ "root", root; "its child", child ]
    ~f:(fun (name, cursor) ->
      let aimed =
        heap_lines
          ~selection:{ Heap_pane.Selection.selected = None; cursor }
          replay
          ~step
      in
      let moved =
        List.filter_mapi (List.zip_exn at_rest aimed) ~f:(fun row (a, b) ->
          match String.equal a b with true -> None | false -> Some row)
      in
      print_s [%message name ~rows_that_changed:(moved : int list)]);
  [%expect
    {|
    (root (rows_that_changed ()))
    ("its child" (rows_that_changed (2 3 4)))
    |}]
;;

let%expect_test "collapsing a structure keeps the others in their column" =
  (* [pack] chooses a column from the footprint a structure has EXPANDED, so
     folding one moves it up its own column and leaves the rest where they
     are. The freed space is still freed — only the sideways placement is
     pinned, which is why the columns below are identical and the rows are
     not. *)
  let replay = replay_of_fixture "set_ops" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  (* each section's header glyph, found where the pane says a click on it
     would toggle that structure *)
  let headers folds =
    List.cartesian_product (List.range 0 40) (List.range 0 60)
    |> List.filter_map ~f:(fun (y, x) ->
      match
        Heap_pane.toggle_at
          ~structures
          ~nodes
          ~new_addresses
          ~folds
          ~scroll:0
          ~pan:0
          ~selection:Heap_pane.Selection.none
          ~width:60
          ~height:40
          ~x
          ~y
      with
      | Some (Heap_pane.Fold.Structure id) -> Some (id, (x, y))
      | Some (Heap_pane.Fold.Node _) | None -> None)
    |> List.sort ~compare:[%compare: int * (int * int)]
  in
  let expanded = headers (Set.empty (module Heap_pane.Fold)) in
  let folded =
    headers
      (Set.of_list (module Heap_pane.Fold) [ Heap_pane.Fold.Structure 1 ])
  in
  let column (id, (x, (_ : int))) = id, x in
  let row ((_ : int), ((_ : int), y)) = y in
  let columns_expanded = List.map expanded ~f:column in
  let columns_folded = List.map folded ~f:column in
  let rows_expanded = List.map expanded ~f:row in
  let rows_folded = List.map folded ~f:row in
  print_s
    [%message
      (columns_expanded : (int * int) list)
        (columns_folded : (int * int) list)
        (rows_expanded : int list)
        (rows_folded : int list)];
  [%expect
    {|
    ((columns_expanded ((1 0) (4 0) (6 0))) (columns_folded ((1 0) (4 0) (6 0)))
     (rows_expanded (25 15 0)) (rows_folded (25 15 0)))
    |}]
;;

let%expect_test "heap fold: toggles sit where the glyphs render" =
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  let toggle ~x ~y =
    Heap_pane.toggle_at
      ~width:56
      ~structures
      ~nodes
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~scroll:0
      ~pan:0
      ~selection:Heap_pane.Selection.none
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
  [%expect {|
    (Structure 2)
    (Node 2())
    ·
    ·
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
       ~heat:(no_heat (calls_of replay))
       ~live:(live_of replay ~step:4)
       ~selected:0
       ~folds:(Int.Set.of_list [ 1 ])
       ~cursor:None
       ~collapsed:false);
  [%expect
    {|
    ▾ CALL STACK                          5 calls · 1 live
     ▸ M.add "a" 1 (M.add "b" 2 M.empty) ⋯ 1
         M.add k (v * 2) acc
         M.add k (v * 2) acc
    ▎▾ M.fold (fun k v acc -> M.add k (v * 2) acc) m
    ▎    M.empty
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
       ~char_range:(10, 23)
       ~collapsed:false);
  [%expect
    {|
    ▾ SOURCE                       map_basic.ml · 10 lines
     ▾  1 (* The canonical positive case: add/add/remove
            fire (3 events);
        2    [empty] (an ident), [find] (returns the
               value) and [ignore] don't. *)
     ▾  3 module M = Map.Make (String)
        4
    ▎▸  5 let () = ⋯ 5 lines
    |}]
;;

let%expect_test "heap fold keeps the rest of the diagram still" =
  (* set a's spine: 1 → r → 2 → r → 3. Folding the "2" card hides "3";
     everything else — the "a · 1" card centered above, the rail, the ∅ —
     must not move a cell, because the folded card keeps its expanded
     footprint *)
  let replay = replay_of_fixture "set_ops" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  let render folds =
    print_view
      ~width:60
      ~height:14
      (Heap_pane.view
         ~note:None
         ~total:None
         ~width:60
         ~height:14
         ~structures
         ~nodes
         ~new_addresses
         ~folds
         ~scroll:0
         ~pan:0
         ~selection:Heap_pane.Selection.none)
  in
  render (Set.empty (module Heap_pane.Fold));
  render
    (Set.of_list (module Heap_pane.Fold) [ Heap_pane.Fold.Node (1, [ 1 ]) ]);
  [%expect
    {|
    HEAP                      3 live · 9 nodes · 288 B · 4 new
    ▾ #6 · set ⟨int⟩ · 4 nodes · 128 B
                      ▾┌ #6  new ┐
                       │3        │
                       └─────────┘
                    ┌──────────┴───────────┐
                    l                      r
           ▾┌ new ┐                ┌ new ┐
            │2    │                │4    │
            └─────┘                └─────┘
             ┌──────┴───────┐
             l              r
     ┌ new ┐              ┌┄┄┄┐
     │1    │              ┆ ∅ ┆
    HEAP                      3 live · 9 nodes · 288 B · 4 new
    ▾ #6 · set ⟨int⟩ · 4 nodes · 128 B
                      ▾┌ #6  new ┐
                       │3        │
                       └─────────┘
                    ┌──────────┴───────────┐
                    l                      r
           ▾┌ new ┐                ┌ new ┐
            │2    │                │4    │
            └─────┘                └─────┘
             ┌──────┴───────┐
             l              r
     ┌ new ┐              ┌┄┄┄┐
     │1    │              ┆ ∅ ┆
    |}]
;;

(* Every glyph the interface draws must be one terminal cell wide, or the
   text after it slides and a card's wash appears to spill past its border.
   Notty measures them here — but the terminal has the final say, and the two
   can disagree.

   So the card interior obeys a second rule this list cannot check: it uses
   only ASCII and glyphs in the same East Asian width class (Ambiguous) as
   the box-drawing characters that frame it. A terminal that widened those
   would visibly shred the whole frame, so it cannot quietly widen the
   contents alone. [→] (U+2192, Ambiguous) is in; [↦] (U+21A6, Neutral) was
   the one exception and is out — a font fallback rendering it double-width
   pushed the closing [│] one cell past the wash. *)
let%expect_test "every drawn glyph is one cell wide" =
  let glyphs =
    [ 0x2500, "─"
    ; 0x2502, "│"
    ; 0x250c, "┌"
    ; 0x2510, "┐"
    ; 0x2514, "└"
    ; 0x2518, "┘"
    ; 0x2534, "┴"
    ; 0x252c, "┬"
    ; 0x253c, "┼"
    ; 0x2501, "━"
    ; 0x258e, "▎"
    ; 0x25be, "▾"
    ; 0x25b8, "▸"
    ; 0x25c2, "◂"
    ; 0x23f5, "⏵"
    ; 0x23f8, "⏸"
    ; 0x00b7, "·"
    ; 0x2192, "→"
    ; 0x2197, "↗"
    ; 0x2504, "┄"
    ; 0x2506, "┆"
    ; 0x21d2, "⇒"
    ; 0x27e8, "⟨"
    ; 0x27e9, "⟩"
    ; 0x22ef, "⋯"
    ; 0x2205, "∅"
    ; 0x25cf, "●"
    ; 0x2588, "█"
    ; 0x2524, "┤"
    ; 0x252c, "┬"
    ; 0x2580, "▀"
    ]
  in
  List.iter glyphs ~f:(fun (scalar, glyph) ->
    let width =
      Bonsai_term.View.uchar_tty_width (Uchar.of_scalar_exn scalar)
    in
    match width with
    | 1 -> ()
    | width ->
      print_s [%message "not one cell" (glyph : string) (width : int)]);
  print_s [%sexp (List.length glyphs : int)];
  [%expect {| 31 |}]
;;

let%expect_test "delta wire: a revisit stub replays the earlier shape" =
  (* map_rewalk's second event is a stub — same id, current address, no
     content — so the pane must draw what that id was defined as *)
  let replay = replay_of_fixture "map_rewalk" in
  heap_view ~height:9 replay ~step:1;
  [%expect
    {|
    HEAP                  2 live · 3 nodes · 168 B · 2 new
    ▾ m · map ⟨string ⇒ int⟩ · 2 nodes · 112 B
    ▾┌ m  new ┐
     │"a" → 1 │
     └────────┘
      ┌──────┴───────┐
      l              r
    ┌┄┄┄┐    ┌── new ┐
    ┆ ∅ ┆    │"b" → 2│
    |}]
;;

let%expect_test "delta wire: a shared payload is drawn once, then pointed at"
  =
  let replay = replay_of_fixture "map_shared_payload" in
  heap_view ~width:64 ~height:30 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                          4 live · 7 nodes · 360 B · 3 new
    ▾ #5 · map ⟨string ⇒ point⟩ · 3 nodes · 168 B
                       ▾┌ #5  new ┐
                        │"p"      │
                        └─────────┘
      ┌──────────────┬──────────┴──────────────────────────┐
      l              d                                     r
    ┌┄┄┄┐    ┌ p ┐                                ▾┌ new ┐
    ┆ ∅ ┆    │x=1│                                 │"q"  │
    └┄┄┄┘    │y=2│                                 └─────┘
             └───┘                  ┌───────────────┬──────┴──────
                                    l               d
                                  ┌┄┄┄┐   ┌ ↗ ┄┄┐
                                  ┆ ∅ ┆   ┆ x=1 ┆
                                  └┄┄┄┘   ┆ y=2 ┆
                                          └┄┄┄┄┄┘

                                                                 ┌
                                                                 ┆
                                                                 └


    ▾ m · map ⟨string ⇒ point⟩ · 2 nodes · 112 B
                    ▾┌ m ┐
                     │"p"│
                     └───┘
      ┌───────────────┬──────┴──────────────────────┐
      l               d                             r
    ┌┄┄┄┐   ┌ ↗ ┄┄┐                        ▾┌───┐
    ┆ ∅ ┆   ┆ x=1 ┆                         │"q"│
    |}]
;;

(* the demo fixture: a five-node tree next to the version derived from it, so
   the two arrows stand for whole subtrees that were not rebuilt *)
let%expect_test "delta wire: one [add] rebuilds a spine and shares the rest" =
  let replay = replay_of_fixture "map_spine_sharing" in
  heap_view ~width:64 ~height:36 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                          2 live · 6 nodes · 336 B · 3 new
    ▾ bigger · map ⟨string ⇒ int⟩ · 3 nodes · 168 B
                            ▾┌ bigger  new ┐
                             │"f" → 6      │
                             └─────────────┘
                    ┌────────────────┴────────────────┐
                    l                                 r
           ▾┌───────┐                        ▾┌── new ┐
            │"d" → 4│                         │"h" → 8│
            └───────┘                         └───────┘
             ┌──────┴───────┐              ┌──────────┴──────────┐
             l              r              l                     r
     ┌───────┐            ┌┄┄┄┐    ┌── new ┐             ┌────────
     │"b" → 2│            ┆ ∅ ┆    │"g" → 7│             │"j" → 10
     └───────┘            └┄┄┄┘    └───────┘             └────────

    ▾ m · map ⟨string ⇒ int⟩ · 3 nodes · 168 B
                ▾┌ m ────┐
                 │"f" → 6│
                 └───────┘
              ┌──────────┴──────────┐
              l                     r
    ┌ ↗ ┄┄┄┄┄┄┐            ▾┌───────┐
    ┆ "d" → 4 ┆             │"h" → 8│
    └┄┄┄┄┄┄┄┄┄┘             └───────┘
                             ┌──────┴───────┐
                             l              r
                           ┌┄┄┄┐    ┌────────┐
                           ┆ ∅ ┆    │"j" → 10│
                           └┄┄┄┘    └────────┘
    |}]
;;

let%expect_test "delta wire: a payload cycle terminates" =
  let replay = replay_of_fixture "queue_cycle" in
  heap_view ~width:64 ~height:30 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                           2 live · 3 nodes · 88 B · 1 new
    ▾ q · queue ⟨cyc⟩ · 2 nodes · 48 B
    ▾┌ q ─────┐
     │length 1│
     └────────┘
             │
           first
    ▾┌── new ┐
     │slots 2│
     └───────┘
             │
             0
     ┌ r ────────┐
     │name="loop"│
     │self=0     │
     └───────────┘
    |}]
;;

let%expect_test "delta wire: version chains share their spines" =
  let replay = replay_of_fixture "map_versions" in
  heap_view ~width:64 ~height:20 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                          2 live · 8 nodes · 448 B · 4 new
    ▾ #11 · map ⟨string ⇒ int⟩ · 4 nodes · 224 B
               ▾┌ #11  new ┐
                │"b" → 2   │
                └──────────┘
             ┌──────────┴──────────┐
             l                     r
     ┌───────┐            ▾┌── new ┐
     │"a" → 1│             │"c" → 3│
     └───────┘             └───────┘
                            ┌──────┴───────┐
                            l              r
                          ┌┄┄┄┐   ▾┌── new ┐
                          ┆ ∅ ┆    │"d" → 4│
                          └┄┄┄┘    └───────┘
                                    ┌──────┴───────┐
                                    l              r
                                  ┌┄┄┄┐    ┌── new ┐
                                  ┆ ∅ ┆    │"e" → 5│
                                  └┄┄┄┘    └───────┘
    |}]
;;

let%expect_test "selection: only the chosen and aimed cards spell an address"
  =
  (* map_spine_sharing's last step: [bigger] is what the step walked, so it
     is selected by default. Aiming one card down lands on the [↗] pointer
     under it — which names [m]'s "d" subtree — so the pointer and the card
     it points at are picked out together, wearing the one address between
     them. Nothing else spells an address, which is what lets the trees fit
     side by side. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let selected = current_spot replay ~step in
  let structures = (Replay.step_exn replay ~step).structures in
  let cursor =
    Heap_pane.move_cursor
      ~structures
      ~nodes:(Replay.step_exn replay ~step).nodes
      ~new_addresses:(Replay.step_exn replay ~step).new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~selection:{ Heap_pane.Selection.selected; cursor = None }
      ~direction:Down
  in
  heap_view
    ~width:76
    ~height:26
    ~selection:{ Heap_pane.Selection.selected; cursor }
    replay
    ~step;
  [%expect
    {|
    HEAP                                      2 live · 6 nodes · 336 B · 3 new
    ▾ bigger · map ⟨string ⇒ int⟩ · 3 nodes · 168 B
                            ▾┌ bigger ─── new ┐
                             │"f" → 6         │
                             └ 0x78de5b5fff78 ┘
                    ┌────────────────┴────────────────┐
                    l                                 r
           ▾┌────────────────┐               ▾┌── new ┐
            │"d" → 4         │                │"h" → 8│
            └ 0x78de5b5e1d10 ┘                └───────┘
             ┌──────┴───────┐              ┌──────────┴──────────┐
             l              r              l                     r
     ┌───────┐            ┌┄┄┄┐    ┌── new ┐             ┌────────┐
     │"b" → 2│            ┆ ∅ ┆    │"g" → 7│             │"j" → 10│
     └───────┘            └┄┄┄┘    └───────┘             └────────┘

    ▾ m · map ⟨string ⇒ int⟩ · 3 nodes · 168 B
                ▾┌ m ────┐
                 │"f" → 6│
                 └───────┘
              ┌──────────┴──────────┐
              l                     r
    ┌ ↗ ┄┄┄┄┄┄┐            ▾┌───────┐
    ┆ "d" → 4 ┆             │"h" → 8│
    └┄┄┄┄┄┄┄┄┄┘             └───────┘
                             ┌──────┴───────┐
    |}]
;;

let%expect_test "selection: [wasd] walks the tree, not the picture" =
  (* map_spine_sharing's last step draws [m]'s five-node tree and the version
     derived from it. [s] descends, [a]/[d] run along a layer — "b" and "j"
     are cousins, two subtrees apart, but they share a depth — and [s] off a
     leaf falls through to the next structure.

     The last three presses are the point: [s] lands on [bigger]'s [↗]
     pointer, which names [m]'s "d" subtree — and [d] then runs along
     [bigger]'s layer and [w] climbs to [bigger]'s root, not [m]'s. Standing
     on a pointer lights up the card it names but leaves the cursor where it
     is, in the tree being read. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  (* name a card the way the screen does: its structure's name if it is a
     root, otherwise the map key it holds. The delta wire keeps interior
     nodes in the id table rather than inline under the root, so that is
     where the keys are looked up. *)
  let label ({ address; site = (_ : Heap_pane.Site.t) } : Heap_pane.Spot.t) =
    match
      List.find structures ~f:(fun (structure : Replay.Structure.t) ->
        Snapshot.Address.equal structure.address address)
    with
    | Some structure -> Replay.Structure.display structure
    | None ->
      Map.data nodes
      |> List.find ~f:(fun (node : Snapshot.Node.t) ->
        Snapshot.Address.equal node.virtual_address address)
      |> Option.bind ~f:(fun (node : Snapshot.Node.t) ->
        List.Assoc.find node.block "v" ~equal:String.equal)
      |> Option.value_map
           ~default:(Snapshot.Address.display address)
           ~f:Snapshot.Block.display
  in
  let selection =
    ref
      { Heap_pane.Selection.selected = current_spot replay ~step
      ; cursor = None
      }
  in
  List.iter
    [ Heap_pane.Direction.Up, "w"
    ; Down, "s"
    ; Right, "d"
    ; Down, "s"
    ; Left, "a"
    ; Left, "a"
    ; Up, "w"
    ; Down, "s"
    ; Down, "s"
    ; Down, "s"
    ; Right, "d"
    ; Up, "w"
    ]
    ~f:(fun (direction, key) ->
      let moved =
        Heap_pane.move_cursor
          ~structures
          ~nodes
          ~new_addresses
          ~folds:(Set.empty (module Heap_pane.Fold))
          ~selection:!selection
          ~direction
      in
      (match moved with
       | None -> ()
       | Some (_ : Heap_pane.Spot.t) ->
         selection := { !selection with cursor = moved });
      let landed =
        match moved with
        | None -> "(nothing that way)"
        | Some spot -> label spot
      in
      print_endline [%string "%{key} -> %{landed}"]);
  [%expect
    {|
    w -> bigger
    s -> bigger
    d -> (nothing that way)
    s -> "d"
    a -> (nothing that way)
    a -> (nothing that way)
    w -> bigger
    s -> "d"
    s -> "b"
    s -> m
    d -> (nothing that way)
    w -> bigger
    |}]
;;

let%expect_test "selection: the cursor can stand on a whole structure" =
  (* A collapsed structure is nothing but its header, so the header has to be
     somewhere the cursor can go — otherwise folding one would put it beyond
     reach. It sits one rung above the tree's root card: [w] off the root
     lands on it, [a]/[d] step across to the structures beside it, and [s]
     descends back into the tree — or lands nowhere when a fold has left
     nothing below the header. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let folded name =
    Set.of_list
      (module Heap_pane.Fold)
      (List.filter_map structures ~f:(fun (structure : Replay.Structure.t) ->
         match String.equal (Replay.Structure.display structure) name with
         | true -> Some (Heap_pane.Fold.Structure structure.id)
         | false -> None))
  in
  let walk ~folds keys =
    let selection =
      ref
        { Heap_pane.Selection.selected = current_spot replay ~step
        ; cursor = None
        }
    in
    List.iter keys ~f:(fun (direction, key) ->
      let moved =
        Heap_pane.move_cursor
          ~structures
          ~nodes
          ~new_addresses
          ~folds
          ~selection:!selection
          ~direction
      in
      Option.iter moved ~f:(fun (_ : Heap_pane.Spot.t) ->
        selection := { !selection with cursor = moved });
      let landed =
        match moved with
        | None -> Sexp.Atom "(nothing that way)"
        | Some { Heap_pane.Spot.site; address = (_ : Snapshot.Address.t) } ->
          [%sexp (site : Heap_pane.Site.t)]
      in
      print_s [%message key ~landed:(landed : Sexp.t)])
  in
  print_endline "-- everything expanded";
  walk
    ~folds:(Set.empty (module Heap_pane.Fold))
    [ Heap_pane.Direction.Up, "w"; Right, "d"; Down, "s" ];
  print_endline "-- with [m] collapsed, its header is all there is of it";
  walk
    ~folds:(folded "m")
    [ Heap_pane.Direction.Up, "w"; Right, "d"; Down, "s" ];
  [%expect
    {|
    -- everything expanded
    (w (landed ((structure 12) (path ()) (is_header true))))
    (d (landed ((structure 9) (path ()) (is_header true))))
    (s (landed ((structure 9) (path ()) (is_header false))))
    -- with [m] collapsed, its header is all there is of it
    (w (landed ((structure 12) (path ()) (is_header true))))
    (d (landed ((structure 9) (path ()) (is_header true))))
    (s (landed "(nothing that way)"))
    |}]
;;

let%expect_test "[h]'s target: the node under the cursor; the whole \
                 structure from its header"
  =
  (* the two glyphs stacked at a section's top left are two different folds —
     the header's hides the structure, the root card's tucks its children —
     and [h] toggles whichever one the cursor is standing on *)
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  let root =
    List.find_exn structures ~f:(fun (s : Replay.Structure.t) ->
      s.is_current)
    |> Heap_pane.spot_of_structure
  in
  print_s [%sexp (Heap_pane.fold_of_spot root : Heap_pane.Fold.t)];
  let header =
    Heap_pane.move_cursor
      ~structures
      ~nodes
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~selection:{ Heap_pane.Selection.selected = Some root; cursor = None }
      ~direction:Heap_pane.Direction.Up
    |> Option.value_exn
  in
  print_s [%sexp (Heap_pane.fold_of_spot header : Heap_pane.Fold.t)];
  [%expect {|
    (Node 2 ())
    (Structure 2)
    |}]
;;

let%expect_test "[h] on a collapsed structure resolves to its header and \
                 reopens it"
  =
  (* stepping drops the cursor, and [h]'s fallback names the walked
     structure's ROOT CARD — which a collapsed structure does not draw.
     Resolving the spot against the canvas lands on the header (the one
     drawing wearing that address), whose fold is the structure itself, so
     the second [h] reopens instead of flipping an invisible node fold. *)
  let replay = replay_of_fixture "queue_basic" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let root = Option.value_exn (current_spot replay ~step) in
  let folded =
    Set.of_list
      (module Heap_pane.Fold)
      [ Heap_pane.Fold.Structure (List.hd_exn structures).Replay.Structure.id
      ]
  in
  let resolved =
    Heap_pane.resolve_spot
      ~structures
      ~nodes
      ~new_addresses
      ~folds:folded
      root
    |> Option.value_exn
  in
  print_s [%sexp (Heap_pane.fold_of_spot root : Heap_pane.Fold.t)];
  print_s [%sexp (Heap_pane.fold_of_spot resolved : Heap_pane.Fold.t)];
  [%expect {|
    (Node 1 ())
    (Structure 1)
    |}]
;;

let%expect_test "accordion: the structure the keyboard is in is the open one"
  =
  (* [z]'s fold set, recomputed from the selection: every structure closed
     but the one the cursor (or, before aiming, the selection) is in. Walking
     off one structure onto the next opens it on arrival and closes the one
     left behind — the canvas is a list of one-line summaries plus wherever
     you are standing. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let effective selection =
    Heap_pane.accordion_folds
      ~structures
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~selection
  in
  let show selection =
    print_view
      ~width:60
      ~height:12
      (Heap_pane.view
         ~note:(Some "accordion")
         ~total:None
         ~width:60
         ~height:12
         ~structures
         ~nodes
         ~new_addresses
         ~folds:(effective selection)
         ~scroll:0
         ~pan:0
         ~selection)
  in
  let selection =
    ref
      { Heap_pane.Selection.selected = current_spot replay ~step
      ; cursor = None
      }
  in
  let move direction =
    let moved =
      Heap_pane.move_cursor
        ~structures
        ~nodes
        ~new_addresses
        ~folds:(effective !selection)
        ~selection:!selection
        ~direction
    in
    Option.iter moved ~f:(fun (_ : Heap_pane.Spot.t) ->
      selection := { !selection with cursor = moved })
  in
  show !selection;
  [%expect
    {|
    HEAP          accordion · 2 live · 6 nodes · 336 B · 3 new
    ▾ bigger · map ⟨string ⇒ int⟩ · 3 nodes · 168 B
                            ▾┌ bigger ─── new ┐
                             │"f" → 6         │
                             └ 0x78de5b5fff78 ┘
                    ┌────────────────┴────────────────┐
                    l                                 r
           ▾┌───────┐                        ▾┌── new ┐
            │"d" → 4│                         │"h" → 8│
            └───────┘                         └───────┘
             ┌──────┴───────┐              ┌──────────┴───────
             l              r              l
    |}];
  (* [w] to the open structure's header, [w] onto the one before it: the
     arrival opens [m], and [bigger] folds up behind us *)
  move Heap_pane.Direction.Up;
  move Heap_pane.Direction.Up;
  show !selection;
  [%expect
    {|
    HEAP          accordion · 2 live · 6 nodes · 336 B · 3 new
    ▾ bigger · map ⟨string ⇒ int⟩ · 3 nodes · 168 B
                            ▾┌ bigger ─── new ┐
                             │"f" → 6         │
                             └ 0x78de5b5fff78 ┘
                    ┌────────────────┴────────────────┐
                    l                                 r
           ▾┌───────┐                        ▾┌── new ┐
            │"d" → 4│                         │"h" → 8│
            └───────┘                         └───────┘
             ┌──────┴───────┐              ┌──────────┴───────
             l              r              l
    |}]
;;

let%expect_test "a step lands the heap on the structure it walked" =
  (* stepping resets the scroll — but to the selection's row, not to the top
     of the canvas: on a dump with hundreds of structures the walked one can
     sit far below the fold, and a pane that opened on the top showed
     everything except the thing the step was about. Aim at the
     highest-address structure, which the address ordering packs last. *)
  let replay = replay_of_fixture "set_ops" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let farthest =
    List.max_elt structures ~compare:(fun (x : Replay.Structure.t) y ->
      Snapshot.Address.compare x.address y.address)
    |> Option.value_exn
  in
  let selection =
    { Heap_pane.Selection.selected =
        Some (Heap_pane.spot_of_structure farthest)
    ; cursor = None
    }
  in
  let scroll, pan =
    Heap_pane.landing
      ~structures
      ~nodes
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~selection
      ~width:44
      ~height:12
  in
  printf "scroll=%d pan=%d\n" scroll pan;
  heap_view ~width:44 ~height:12 ~scroll ~pan ~selection replay ~step;
  [%expect
    {|
    scroll=37 pan=0
     HEAP     5 live · 12 nodes · 384 B · 2 new
     ┆ ∅ ┆    │4│
     └┄┄┄┘    └─┘

     ▾ a · set ⟨int⟩ · 3 nodes · 96 B
     ▾┌ a ─────────────┐
      │1               │
      └ 0x768f6ebf2148 ┘
       ┌──────┴───────┐
       l              r
     ┌┄┄┄┐   ▾┌─┐
     ┆ ∅ ┆    │2│
    |}]
;;

let%expect_test "the heap pans by hand, and the cursor still overrides" =
  (* [\[]/[\]] slide the window sideways across a canvas wider than the pane;
     with nothing aimed at, the offset is exactly where the hand left it
     (clamped to the canvas), and aiming pulls the window back only far
     enough to show the card *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  heap_view ~width:36 ~height:8 replay ~step;
  heap_view ~width:36 ~height:8 ~pan:12 replay ~step;
  (* far past the canvas edge: clamps to the right edge, not to nowhere *)
  heap_view ~width:36 ~height:8 ~pan:1000 replay ~step;
  (* aiming wins over the hand: the same wild pan lands wherever shows the
     cursor's card — which, being the cursor, spells out its address *)
  heap_view
    ~width:36
    ~height:8
    ~pan:1000
    ~selection:
      { Heap_pane.Selection.selected = None
      ; cursor = current_spot replay ~step
      }
    replay
    ~step;
  [%expect
    {|
    HEAP 2 live · 6 nodes · 336 B · 3 n
    ▾ bigger · map ⟨string ⇒ int⟩ · 3
                            ▾┌ bigger
                             │"f" → 6
                             └────────
                    ┌────────────────┴
                    l
           ▾┌───────┐
    HEAP 2 live · 6 nodes · 336 B · 3 n
    ap ⟨string ⇒ int⟩ · 3 nodes · 168
                ▾┌ bigger  new ┐
                 │"f" → 6      │
                 └─────────────┘
        ┌────────────────┴────────────
        l
    ────┐                        ▾┌──
    HEAP 2 live · 6 nodes · 336 B · 3 n
     · 3 nodes · 168 B
    gger  new ┐
     → 6      │
    ──────────┘
    ────┴────────────────┐
                         r
                ▾┌── new ┐
    HEAP 2 live · 6 nodes · 336 B · 3 n
    string ⇒ int⟩ · 3 nodes · 168 B
            ▾┌ bigger ─── new ┐
             │"f" → 6         │
             └ 0x78de5b5fff78 ┘
    ┌────────────────┴────────────────
    l
    ┐                        ▾┌── new
    |}]
;;

let%expect_test "filter: only matching structures stay on the canvas" =
  (* [/]'s cut, by the header's own words — name, kind, type — and the meta
     line owns up to what it is hiding *)
  let replay = replay_of_fixture "queue_of_maps" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  let show filter =
    let kept =
      List.filter structures ~f:(Heap_pane.matches_filter ~filter)
    in
    print_view
      ~width:56
      ~height:6
      (Heap_pane.view
         ~note:(Some [%string "/%{filter}"])
         ~total:(Some (List.length structures))
         ~width:56
         ~height:6
         ~structures:kept
         ~nodes
         ~new_addresses
         ~folds:(Set.empty (module Heap_pane.Fold))
         ~scroll:0
         ~pan:0
         ~selection:Heap_pane.Selection.none)
  in
  show "queue";
  [%expect
    {|
    HEAP      /queue · 1 of 2 live · 1 node · 24 B · 1 new
    ▾ q · queue ⟨int M.t⟩ · 1 node · 24 B
     ┌ q  new ┐
     │length 0│
     └────────┘
    |}];
  (* matching is case-insensitive, and the kind is part of the header's
     words, so [/MAP] finds the map *)
  show "MAP";
  [%expect
    {|
    HEAP        /MAP · 1 of 2 live · 1 node · 56 B · 1 new
    ▾ m · map ⟨string ⇒ int⟩ · 1 node · 56 B
     ┌ m ────┐
     │"k" → 1│
     └───────┘
    |}]
;;

let%expect_test "selection: committing a link follows the node to its step" =
  (* [Enter] on a [↗] pointer jumps the replay to where that node was
     allocated — a step at which the structure the pointer lived in does not
     exist yet, so the site it named is gone. The chosen card has to follow
     the node to whatever tree draws it there rather than vanishing — to the
     node's own card, not to another pointer at it. [add "b" 2 m3] rebuilt
     the spine, so the "d" node the pointer named was allocated here, in the
     structure at the bottom: that is the card wearing the address. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let last = Replay.length replay - 1 in
  let folds = Set.empty (module Heap_pane.Fold) in
  let pointer =
    let { Replay.Step.structures; nodes; new_addresses; _ } =
      Replay.step_exn replay ~step:last
    in
    Heap_pane.move_cursor
      ~structures
      ~nodes
      ~new_addresses
      ~folds
      ~selection:
        { Heap_pane.Selection.selected = current_spot replay ~step:last
        ; cursor = None
        }
      ~direction:Down
    |> Option.value_exn
  in
  let birth =
    List.init (Replay.length replay) ~f:Fn.id
    |> List.find_exn ~f:(fun step ->
      Set.mem
        (Replay.step_exn replay ~step).new_addresses
        pointer.Heap_pane.Spot.address)
  in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:birth
  in
  let resolved =
    Heap_pane.resolve_spot ~structures ~nodes ~new_addresses ~folds pointer
  in
  print_s [%message (birth : int)];
  heap_view
    ~width:64
    ~height:14
    ~scroll:22
    ~selection:{ Heap_pane.Selection.selected = resolved; cursor = None }
    replay
    ~step:birth;
  [%expect
    {|
    (birth 3)
     HEAP                          4 live · 8 nodes · 448 B · 3 new
      │"d" → 4│             │"h" → 8│
      └───────┘             └───────┘

     ▾ m2 · map ⟨string ⇒ int⟩ · 2 nodes · 112 B
            ▾┌ m2 ───┐
             │"f" → 6│
             └───────┘
              ┌──────┴───────┐
              l              r
      ┌───────┐            ┌┄┄┄┐
      │"d" → 4│            ┆ ∅ ┆
      └───────┘            └┄┄┄┘
    |}]
;;

let%expect_test "stack pane: the aimed row rides over the selected one" =
  (* [w] from the live frame aims at the call above it; the live row keeps
     its own bar, so both are readable at once *)
  let replay = replay_of_fixture "map_fold" in
  let calls = calls_of replay in
  let live = live_of replay ~step:2 in
  let cursor =
    Stack_pane.move_cursor
      ~calls
      ~live
      ~selected:1
      ~folds:Int.Set.empty
      ~cursor:None
      ~direction:`Up
  in
  print_s [%message (cursor : int option)];
  print_view
    ~height:8
    (Stack_pane.view
       ~width:56
       ~height:8
       ~calls
       ~heat:(no_heat calls)
       ~live
       ~selected:1
       ~folds:Int.Set.empty
       ~cursor
       ~collapsed:false);
  [%expect
    {|
    (cursor (1))
     ▾ CALL STACK                          5 calls · 2 live
          M.add "b" 2 M.empty
     ▎▾ M.add "a" 1 (M.add "b" 2 M.empty)
     ▎    M.add k (v * 2) acc
          M.add k (v * 2) acc
      ▾ M.fold (fun k v acc -> M.add k (v * 2) acc) m
          M.empty
    |}]
;;

let%expect_test "stack pane: heat colors the callee names, layout untouched" =
  let replay = replay_of_fixture "map_fold" in
  let calls = calls_of replay in
  (* a synthetic profile join spanning the ramp, plus a call the profile has
     no data on (it keeps its ordinary color). Heat lives entirely in the
     text color, so the dumb-cap picture must match the no-heat layout except
     for the header's [· heat] — color itself is asserted on [Theme.heat]
     below. *)
  let heat =
    Array.mapi calls ~f:(fun step (_ : Call.t) ->
      match step with
      | 0 -> Some 0.4
      | 1 -> Some 0.12
      | 2 -> Some 0.05
      | 3 -> Some 0.015
      | _ -> None)
  in
  print_view
    ~height:10
    (Stack_pane.view
       ~width:56
       ~height:10
       ~calls
       ~heat
       ~live:(live_of replay ~step:2)
       ~selected:1
       ~folds:Int.Set.empty
       ~cursor:None
       ~collapsed:false);
  [%expect
    {|
    ▾ CALL STACK                   5 calls · 2 live · heat
         M.add "b" 2 M.empty
     ▾ M.add "a" 1 (M.add "b" 2 M.empty)
    ▎    M.add k (v * 2) acc
         M.add k (v * 2) acc
     ▾ M.fold (fun k v acc -> M.add k (v * 2) acc) m
         M.empty
    |}]
;;

let%expect_test "session bar: the heat legend appears only with a profile" =
  print_view
    ~width:80
    ~height:1
    (Session_bar.view
       ~width:80
       ~dump_name:"greet.dump"
       ~structure:"Map"
       ~heat:true);
  [%expect
    {| ● ocaml-debug │ greet.dump │ Map · replay                  heat █████ cold→hot |}];
  print_view
    ~width:80
    ~height:1
    (Session_bar.view
       ~width:80
       ~dump_name:"greet.dump"
       ~structure:"Map"
       ~heat:false);
  [%expect {| ● ocaml-debug │ greet.dump │ Map · replay |}]
;;

let%expect_test "heat ramp buckets are log-spaced" =
  List.iter [ 0.25; 0.1; 0.05; 0.02; 0.001 ] ~f:(fun share ->
    let color = Theme.heat ~share in
    let index, (_ : Bonsai_term.Attr.Color.t) =
      Array.findi_exn Theme.heat_ramp ~f:(fun (_ : int) stop ->
        phys_equal stop color)
    in
    print_endline [%string "%{share#Float} -> ramp %{index#Int}"]);
  [%expect
    {|
    0.25 -> ramp 4
    0.1 -> ramp 3
    0.05 -> ramp 2
    0.02 -> ramp 1
    0.001 -> ramp 0
    |}]
;;
