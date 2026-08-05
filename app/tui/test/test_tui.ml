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

(* the pane the way it draws with nothing chosen and nothing aimed at: no row
   spells out its address, which is the common case on screen *)
let heap_view
  ?(width = 56)
  ?(height = 15)
  ?(scroll = 0)
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
       ~selection)
;;

(* the [Enter] pop-out: the structure this step walked, drawn as the diagram
   it physically is rather than as the outline that stands for it *)
let diagram_view
  ?(width = 64)
  ?(height = 18)
  ?(scroll = 0)
  ?(pan = 0)
  replay
  ~step
  =
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let structure =
    List.find_exn structures ~f:(fun (structure : Replay.Structure.t) ->
      structure.is_current)
  in
  print_view
    ~width
    ~height
    (Heap_pane.Diagram.view
       ~structure
       ~structures
       ~nodes
       ~new_addresses
       ~width
       ~height
       ~scroll
       ~pan)
;;

(* the structure this step walked — what the app selects by default, and so
   the row that shows its address *)
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
       ~cursor:None);
  [%expect
    {|
    CALL STACK                            5 calls · 2 live
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
    HEAP                          2 live · 3 nodes · 2 new
    ▾ m  int M.t  1 binding
    └─   "a" → 1
    ▾ m  int M.t  2 bindings  new
    ├─   "a" → 1  new
    └─   "b" → 2  new
    |}]
;;

let%expect_test "heap pane: a queue chains cells off first/next" =
  let replay = replay_of_fixture "queue_basic" in
  heap_view replay ~step:2;
  [%expect
    {|
    HEAP                          1 live · 3 nodes · 1 new
    ▾ q  string Queue.t  length 2
    ├─   "x"
    └─   "y"  new
    |}]
;;

let%expect_test "heap pane: boxed map data becomes a d→ child" =
  let replay = replay_of_fixture "map_data_kinds" in
  heap_view replay ~step:2;
  [%expect
    {|
    HEAP                          3 live · 5 nodes · 2 new
    ▾ m  float M.t  1 binding
    └─   "pi" → 3.14
    ▾ #2  float M.t  2 bindings
    ├─   "pi" → 3.14
    └─   "e" → 2.71
    ▾ #4  (int * string) M.t  1 element  new
    └─ ▾ "pair" →  new
       └─   d  1, "one"  new
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
    HEAP                           1 live · 1 node · 1 new
    ▾ #1  int M.t  1 binding  new
    └─   "dead" → 0  new
    HEAP                           1 live · 1 node · 1 new
    ▾ #2  int M.t  1 binding  new
    └─   "live" → 1  new
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
    HEAP                              3 live · 8 nodes · 3 new
    ▾ m  (string, int) Map.t  1 binding
    └─   "b" → 2
    ▾ m  (string, int) Map.t  2 bindings
    ├─   "b" → 2
    └─   "a" → 1
    ▾ m  (string, int) Map.t  3 bindings  new
    ├─   "b" → 2  new
    ├─   ↗ "a" → 1
    └─   "c" → 3  new
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
    HEAP                                  1 live · 9 nodes · 2 new
    ▾ q  (string, int) Hash_queue.t  4 bindings
    ├─   v  "a" → 1
    ├─   v  "b" → 2
    ├─   v  "c" → 3  new
    └─   ↗ null
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
    HEAP                              3 live · 5 nodes · 1 new
      p  point  x=3  y=4
    ▾ ts  trades  0  new
    └─ ▾ hd  t  trade  101
       ├─   tags  "buy", "limit"
       └─   span  1, 9
    |}]
;;

let%expect_test "a wrapped record breaks between its fields, never inside \
                 one"
  =
  (* [x=3] is two spans — a muted label and a colored value — with nothing
     between them, and a break at that seam would leave [x=] ending one line
     and its [3] beginning the next. A change of color is not a place to
     break, so the whole field moves down together. *)
  let replay = replay_of_fixture "user_types" in
  heap_view ~width:16 ~height:12 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP 3 live · 5
      p  point
        x=3  y=4
    ▾ ts  trades
        0  new
    └─ ▾ hd  t
           trade
           101
       ├─   tags
       │      "buy
       │      "lim
       └─   span
    |}]
;;

let%expect_test "the pop-out draws the tree the outline stands for" =
  (* the same map the outline reads as two bindings, as the AVL nodes it
     actually is: a root with an empty [l] and a walked [r], each edge
     labeled where the wire labeled it. This is the whole point of the
     pop-out — the shape is the thing the outline is deliberately not
     showing. *)
  let replay = replay_of_fixture "map_basic" in
  diagram_view replay ~step:1;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────┐
    │ DIAGRAM                     m · int M.t · 2 nodes · esc back │
    │                                                              │
    │                                                              │
    │                                                              │
    │                        ┌ m  new ┐                            │
    │                        │"a" → 1 │                            │
    │                        └────────┘                            │
    │                        ┌────┴────┐                           │
    │                        l         r                           │
    │                      ┌┄┄┄┐   ┌── new ┐                       │
    │                      ┆ ∅ ┆   │"b" → 2│                       │
    │                      └┄┄┄┘   └───────┘                       │
    │                                                              │
    │                                                              │
    │                                                              │
    │                                                              │
    └──────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "the pop-out draws the whole structure, shared nodes and all"
  =
  (* Six bindings in an AVL tree three levels deep, and the pop-out draws
     every node of it — including the ones this version did not allocate,
     which the outline lists as [\u2197] pointers because they are already on
     the pane under the version that did. In here there is only one
     structure, so there is nothing to point at, and the count is what was
     drawn rather than what this version's own snapshot defines.

     A node reached twice WITHIN one structure is still a pointer box, which
     is also what stops a cycle. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  diagram_view ~width:78 ~height:18 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────┐
    │ DIAGRAM                              bigger · int M.t · 6 nodes · esc back │
    │                                                                            │
    │                             ┌ bigger  new ┐                                │
    │                             │"f" → 6      │                                │
    │                             └─────────────┘                                │
    │                          ┌─────────┴──────────┐                            │
    │                          l                    r                            │
    │                      ┌───────┐            ┌── new ┐                        │
    │                      │"d" → 4│            │"h" → 8│                        │
    │                      └───────┘            └───────┘                        │
    │                     ┌────┴────┐         ┌─────┴──────┐                     │
    │                     l         r         l            r                     │
    │                 ┌───────┐   ┌┄┄┄┐   ┌── new ┐   ┌────────┐                 │
    │                 │"b" → 2│   ┆ ∅ ┆   │"g" → 7│   │"j" → 10│                 │
    │                 └───────┘   └┄┄┄┘   └───────┘   └────────┘                 │
    │                                                                            │
    └────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "the pop-out follows a reference into the structure it names"
  =
  (* [Queue.add m q] puts a tracked map inside a tracked queue, and the
     diagram follows the reference the way the outline nests it: the queue's
     cells, then the map's own tree hanging off the cell that holds it, named
     on its top box. *)
  let replay = replay_of_fixture "queue_of_maps" in
  diagram_view ~width:78 ~height:22 replay ~step:2;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────┐
    │ DIAGRAM                           q · int M.t Queue.t · 3 nodes · esc back │
    │                                                                            │
    │                                                                            │
    │                                                                            │
    │                                 ┌ q ─────┐                                 │
    │                                 │length 1│                                 │
    │                                 └────────┘                                 │
    │                                      │                                     │
    │                                    first                                   │
    │                                  ┌── new ┐                                 │
    │                                  │slots 2│                                 │
    │                                  └───────┘                                 │
    │                                      │                                     │
    │                                      0                                     │
    │                                  ┌ m ────┐                                 │
    │                                  │"k" → 1│                                 │
    │                                  └───────┘                                 │
    │                                                                            │
    │                                                                            │
    │                                                                            │
    └────────────────────────────────────────────────────────────────────────────┘
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
    SOURCE                         map_basic.ml · 10 lines
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
       ~char_range:(0, 0));
  [%expect
    {|
    SOURCE           gone.ml · missing

     lib/gone.ml is not at
     ./lib/gone.ml — the dump's paths
     resolve from the replayed
     program's root, so run there or
     pass -source-root DIR
    |}]
;;

let%expect_test "transport: ticks, then the clickable key legend" =
  print_view
    ~width:96
    ~height:3
    (Transport.view
       ~width:96
       ~step:1
       ~total:3
       ~playing:false
       ~accordion:false
       ~diagram:false);
  [%expect
    {|
    ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    ◂ back · step ▸ · [space] play · ↑↓ node · ⏎ diagram · h fold · z accordion · / filter · q quit
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
    SOURCE              map_fold.ml · 15 lines
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
    HEAP                              3 live · 9 nodes · 4 new
    ▾ a  S.t  3 elements
    ├─   1
    ├─   2
    └─   3
    ▾ b  S.t  2 elements
    ├─   3
    └─   4
    ▾ #6  S.t  4 elements  new
    ├─   3  new
    ├─   2  new
    ├─   1  new
    └─   4  new
    |}]
;;

let%expect_test "heap clicks land on a row edge to edge, and nowhere past it"
  =
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
      ~selection:Heap_pane.Selection.none
      ~height:15
      ~x
      ~y
    |> Option.value_map ~default:"·" ~f:(fun (spot : Heap_pane.Spot.t) ->
      Snapshot.Address.display spot.address)
  in
  (* a row spans the pane, so the far right of a line is still that line and
     only [y] decides; the second structure's row is a place of its own; and
     below the last row there is nothing to land on *)
  print_endline (at ~x:2 ~y:0);
  print_endline (at ~x:48 ~y:0);
  print_endline (at ~x:2 ~y:2);
  print_endline (at ~x:2 ~y:9);
  [%expect
    {|
    0x7fa801ff2348
    0x7fa801ff2348
    0x7fa801fee750
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
    HEAP                          2 live · 2 nodes · 1 new
    ▾ m  int M.t  1 binding
    └─   "k" → 1
      q  int M.t Queue.t  length 0  new
    |}]
;;

let%expect_test "heap pane: Queue.add links the map into the queue's tree" =
  (* the cell's content is (Id 1) — the registry resolves it, so the map's
     tree hangs off the cell's v→ edge, tagged #1 *)
  let replay = replay_of_fixture "queue_of_maps" in
  heap_view ~height:21 replay ~step:2;
  [%expect
    {|
    HEAP                          2 live · 3 nodes · 1 new
    ▾ q  int M.t Queue.t  length 1
    └─ ▾ m  int M.t  1 binding
       └─   "k" → 1
    |}]
;;

let%expect_test "heap pane: hashtbl walks record → bucket array → chain" =
  let replay = replay_of_fixture "hashtbl_basic" in
  heap_view ~width:60 ~height:17 replay ~step:1;
  [%expect
    {|
    HEAP                              1 live · 3 nodes · 1 new
    ▾ tbl  (string, int) Hashtbl.t  size 1
    └─   "a" → 1  new
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
    HEAP                              2 live · 3 nodes · 1 new
    ▾ qq  'a Queue.t Queue.t  length 1
    └─   q1  'a Queue.t  length 0
    |}]
;;

let%expect_test "heap pane: closures stay opaque" =
  let replay = replay_of_fixture "queue_of_closures" in
  heap_view ~width:60 ~height:13 replay ~step:1;
  [%expect
    {|
    HEAP                              1 live · 2 nodes · 2 new
    ▾ q  (int -> int) Queue.t  length 1
    └─   ⟨0x73a5227efa18⟩  new
    |}]
;;

let%expect_test "control chips hit-test exactly where they render" =
  (* the row wants 96 columns now that [⏎ diagram] is on it; narrower than
     that and [start_column] pins it at 0 and the right-hand chips crop *)
  let width = 96 in
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
    (Node 33 39)
    (Diagram 43 51)
    (Fold 55 60)
    (Accordion 64 74)
    (Filter 78 85)
    (Quit 89 94)
    |}]
;;

let%expect_test "heap fold: a row keeps itself, hides its kids" =
  let replay = replay_of_fixture "queue_of_maps" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  (* the map nests under the queue element it became; folding it leaves the
     row that says what it is and takes away the binding under it *)
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
         (Set.of_list (module Heap_pane.Fold) [ Heap_pane.Fold.Structure 1 ])
       ~scroll:0
       ~selection:Heap_pane.Selection.none);
  [%expect
    {|
    HEAP                          2 live · 3 nodes · 1 new
    ▾ q  int M.t Queue.t  length 1
    └─ ▸ m  int M.t  1 binding  ⋯ 1
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
       ~selection:Heap_pane.Selection.none);
  [%expect
    {|
    HEAP                          2 live · 3 nodes · 1 new
    ▸ q  int M.t Queue.t  length 1  ⋯ 2
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
       ~selection)
  |> fun image ->
  let buffer = Buffer.create 1024 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (width, height) image;
  Buffer.contents buffer |> String.split_lines |> List.map ~f:String.rstrip
;;

let%expect_test "aiming lights the one row it lands on, and no other" =
  (* The address rides the right margin, so revealing one cannot move
     anything to its left. Nothing else on the pane may change a cell, which
     is what these line numbers are: exactly the line being aimed at differs.
     (Row 0 is the pane's own header.) *)
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
    (root (rows_that_changed (1)))
    ("its child" (rows_that_changed (2)))
    |}]
;;

let%expect_test "collapsing a structure moves the rows below it up, only up" =
  (* An outline is one column, so a fold can only ever take lines away: every
     structure's glyph stays in column 0 whatever is folded, and the rows
     below the folded one close up by exactly what it was hiding. *)
  let replay = replay_of_fixture "set_ops" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  (* each structure's own glyph, found where the pane says a click on it
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
     (rows_expanded (0 4 7)) (rows_folded (0 1 4)))
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
      ~selection:Heap_pane.Selection.none
      ~height:12
      ~x
      ~y
    |> Option.value_map ~default:"·" ~f:(fun fold ->
      Sexp.to_string [%sexp (fold : Heap_pane.Fold.t)])
  in
  (* #1's own glyph; the binding under it is a leaf, so no glyph; #2's own
     glyph two lines down; a cell that is not the glyph column; past the last
     row *)
  print_endline (toggle ~x:0 ~y:0);
  print_endline (toggle ~x:0 ~y:1);
  print_endline (toggle ~x:0 ~y:2);
  print_endline (toggle ~x:3 ~y:2);
  print_endline (toggle ~x:0 ~y:9);
  [%expect {|
    (Structure 1)
    ·
    (Structure 2)
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
       ~cursor:None);
  [%expect
    {|
    CALL STACK                            5 calls · 1 live
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
       ~char_range:(10, 23));
  [%expect
    {|
    SOURCE                         map_basic.ml · 10 lines
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
  (* folding [b] takes away exactly the two elements it was listing: [a]
     above it does not move a cell, and the union below closes up by two *)
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
         ~selection:Heap_pane.Selection.none)
  in
  render (Set.empty (module Heap_pane.Fold));
  render (Set.of_list (module Heap_pane.Fold) [ Heap_pane.Fold.Structure 4 ]);
  [%expect
    {|
    HEAP                              3 live · 9 nodes · 4 new
    ▾ a  S.t  3 elements
    ├─   1
    ├─   2
    └─   3
    ▾ b  S.t  2 elements
    ├─   3
    └─   4
    ▾ #6  S.t  4 elements  new
    ├─   3  new
    ├─   2  new
    ├─   1  new
    └─   4  new
    HEAP                              3 live · 9 nodes · 4 new
    ▾ a  S.t  3 elements
    ├─   1
    ├─   2
    └─   3
    ▸ b  S.t  2 elements  ⋯ 2
    ▾ #6  S.t  4 elements  new
    ├─   3  new
    ├─   2  new
    ├─   1  new
    └─   4  new
    |}]
;;

(* Every glyph the interface draws must be one terminal cell wide, or the
   text after it slides and a row's columns stop lining up with the rows
   above and below. Notty measures them here — but the terminal has the final
   say, and the two can disagree.

   So a row obeys a second rule this list cannot check: it uses only ASCII
   and glyphs in the same East Asian width class (Ambiguous) as the
   box-drawing characters that draw the tree guides. A terminal that widened
   those would visibly shred every guide on the pane, so it cannot quietly
   widen a value alone. [→] (U+2192, Ambiguous) is in; [↦] (U+21A6, Neutral)
   was the one exception and is out — a font fallback rendering it
   double-width pushed everything after it a cell along. *)
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
      (* the heap outline's tree guides, and the key the footer names for
         walking them *)
    ; 0x251c, "├"
    ; 0x2191, "↑"
    ; 0x2193, "↓"
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
  [%expect {| 34 |}]
;;

let%expect_test "delta wire: a revisit stub replays the earlier shape" =
  (* map_rewalk's second event is a stub — same id, current address, no
     content — so the pane must draw what that id was defined as *)
  let replay = replay_of_fixture "map_rewalk" in
  heap_view ~height:9 replay ~step:1;
  [%expect
    {|
    HEAP                          2 live · 3 nodes · 2 new
    ▾ #1  int M.t  1 binding
    └─   "a" → 1
    ▾ m  int M.t  2 bindings  new
    ├─   "a" → 1  new
    └─   "b" → 2  new
    |}]
;;

let%expect_test "delta wire: a shared payload is drawn once, then pointed at"
  =
  let replay = replay_of_fixture "map_shared_payload" in
  heap_view ~width:64 ~height:30 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                                  4 live · 7 nodes · 3 new
    ▾ #2  point M.t  1 element
    └─ ▾ "p" →
       └─   d  p  point  x=1  y=2
    ▾ m  point M.t  2 elements
    ├─ ▾ "p" →
    │  └─   d  ↗ x=1  y=2
    └─ ▾ "q" →
       └─   d  ↗ x=1  y=2
    ▾ #5  point M.t  3 elements  new
    ├─ ▾ "p" →  new
    │  └─   d  ↗ x=1  y=2
    ├─ ▾ "q" →  new
    │  └─   d  ↗ x=1  y=2
    └─ ▾ "r" →  new
       └─   d  ↗ x=1  y=2
    |}]
;;

(* the demo fixture: a five-node tree next to the version derived from it, so
   the two arrows stand for whole subtrees that were not rebuilt *)
let%expect_test "delta wire: one [add] rebuilds a spine and shares the rest" =
  let replay = replay_of_fixture "map_spine_sharing" in
  heap_view ~width:64 ~height:36 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                                  2 live · 6 nodes · 3 new
    ▾ m  int M.t  5 bindings
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10
    ▾ bigger  int M.t  5 bindings  new
    ├─   "f" → 6  new
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
    ├─   "g" → 7  new
    └─   ↗ "j" → 10
    |}]
;;

let%expect_test "delta wire: a payload cycle terminates" =
  let replay = replay_of_fixture "queue_cycle" in
  heap_view ~width:64 ~height:30 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                                  2 live · 3 nodes · 1 new
    ▾ q  cyc Queue.t  length 1
    └─   r  cyc  name="loop"  self=0
    |}]
;;

let%expect_test "delta wire: version chains share their spines" =
  let replay = replay_of_fixture "map_versions" in
  heap_view ~width:64 ~height:20 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                                  2 live · 8 nodes · 4 new
    ▾ #7  int M.t  4 bindings
    ├─   "b" → 2
    ├─   "a" → 1
    ├─   "c" → 3
    └─   "d" → 4
    ▾ #11  int M.t  5 bindings  new
    ├─   "b" → 2  new
    ├─   ↗ "a" → 1
    ├─   "c" → 3  new
    ├─   "d" → 4  new
    └─   "e" → 5  new
    |}]
;;

let%expect_test "selection: only the chosen and aimed rows spell an address" =
  (* map_spine_sharing's last step: [bigger] is what the step walked, so it
     is selected by default. Aiming one row down lands on that structure's
     first binding, which IS its root — so the structure's row and the row it
     points at are picked out together, wearing the one address between them.
     Nothing else spells an address, which is what lets the trees fit side by
     side. *)
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
    HEAP                                              2 live · 6 nodes · 3 new
    ▾ m  int M.t  5 bindings
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10
    ▾ bigger  int M.t  5 bindings  new                          0x78de5b5fff78
    ├─   "f" → 6  new                                           0x78de5b5fff78
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
    ├─   "g" → 7  new
    └─   ↗ "j" → 10
    |}]
;;

let%expect_test "selection: the cursor walks the outline, along it and into \
                 it"
  =
  (* map_spine_sharing's last step lists [m]'s five bindings and the version
     derived from it. [w]/[s] step to the line above and below and cross the
     boundary between the two structures without being asked to; [a] climbs
     to the line this one hangs under and [d] drops into the first line under
     it.

     [bigger] shares [m]'s subtrees, so its first binding IS its root — which
     is why [d] off [bigger] lands on something the screen still calls
     [bigger] — and the lines under it are [↗] rows naming [m]'s bindings.
     Standing on one lights up the row it names but leaves the cursor where
     it is, in the structure being read. The last [d] is the point: a row
     with nothing under it goes nowhere, rather than falling sideways. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  (* name a row the way the screen does: its structure's name if it is a
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
    w -> "j"
    s -> bigger
    d -> bigger
    s -> "d"
    a -> bigger
    a -> (nothing that way)
    w -> "j"
    s -> bigger
    s -> bigger
    s -> "d"
    d -> (nothing that way)
    w -> bigger
    |}]
;;

let%expect_test "selection: the cursor can stand on a whole structure" =
  (* A collapsed structure is nothing but its own row, so that row has to be
     somewhere the cursor can go — otherwise folding one would put it beyond
     reach. Expanded, [w] off a binding walks up through the bindings above
     it; collapsed, [m]'s row is the whole of [m] and the first line of the
     pane, so [w] reaches it and then has nowhere left to go. *)
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
    [ Heap_pane.Direction.Up, "w"; Up, "w"; Down, "s" ];
  print_endline "-- with [m] collapsed, its header is all there is of it";
  walk
    ~folds:(folded "m")
    [ Heap_pane.Direction.Up, "w"; Up, "w"; Down, "s" ];
  [%expect
    {|
    -- everything expanded
    (w (landed ((structure 9) (path (1 1)) (is_header false))))
    (w (landed ((structure 9) (path (1)) (is_header false))))
    (s (landed ((structure 9) (path (1 1)) (is_header false))))
    -- with [m] collapsed, its header is all there is of it
    (w (landed ((structure 9) (path ()) (is_header true))))
    (w (landed "(nothing that way)"))
    (s (landed ((structure 12) (path ()) (is_header true))))
    |}]
;;

let%expect_test "[h]'s target: the node under the cursor; the whole \
                 structure from its header"
  =
  (* a structure's own row and a row inside one are two different folds — the
     structure's hides the whole thing, a row's tucks its children away — and
     [h] toggles whichever one the cursor is standing on *)
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
    (Structure 2)
    (Node 1 ())
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
    HEAP                  accordion · 2 live · 6 nodes · 3 new
    ▸ m  int M.t  5 bindings  ⋯ 5
    ▾ bigger  int M.t  5 bindings  new          0x78de5b5fff78
    ├─   "f" → 6  new
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
    ├─   "g" → 7  new
    └─   ↗ "j" → 10
    |}];
  (* [w] to the open structure's header, [w] onto the one before it: the
     arrival opens [m], and [bigger] folds up behind us *)
  move Heap_pane.Direction.Up;
  move Heap_pane.Direction.Up;
  show !selection;
  [%expect
    {|
    HEAP                  accordion · 2 live · 6 nodes · 3 new
    ▾ m  int M.t  5 bindings                    0x78de6b6e3738
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10
    ▸ bigger  int M.t  5 bindings  ⋯ 5  new     0x78de5b5fff78
    |}]
;;

let%expect_test "a row wider than the pane wraps, and is still one row" =
  (* Nothing runs off the edge and nothing pans: a row too wide breaks onto
     continuation lines, hanging under its own first column so the guides
     still read down the page.

     The address is placed after the wrapping is settled — the last line of
     the row if it has room, a line of its own if it does not — so revealing
     one cannot reflow the row it belongs to. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  heap_view ~width:34 ~height:12 replay ~step;
  heap_view
    ~width:34
    ~height:12
    ~selection:
      { Heap_pane.Selection.selected = None
      ; cursor = current_spot replay ~step
      }
    replay
    ~step;
  [%expect
    {|
    HEAP    2 live · 6 nodes · 3 new
    ▾ m  int M.t  5 bindings
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10
    ▾ bigger  int M.t  5 bindings
        new
    ├─   "f" → 6  new
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
    HEAP    2 live · 6 nodes · 3 new
    ▾ m  int M.t  5 bindings
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10
    ▾ bigger  int M.t  5 bindings
        new           0x78de5b5fff78
    ├─   "f" → 6  new
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
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
         ~selection:Heap_pane.Selection.none)
  in
  show "queue";
  [%expect
    {|
    HEAP             /queue · 1 of 2 live · 1 node · 1 new
      q  int M.t Queue.t  length 0  new
    |}];
  (* matching is case-insensitive, and the kind is part of the header's
     words, so [/MAP] finds the map *)
  show "MAP";
  [%expect
    {|
    HEAP               /MAP · 1 of 2 live · 1 node · 1 new
    ▾ m  int M.t  1 binding
    └─   "k" → 1
    |}]
;;

let%expect_test "selection: committing a link follows the node to its step" =
  (* [Enter] on a [↗] pointer jumps the replay to where that node was
     allocated — a step at which the structure the pointer lived in does not
     exist yet, so the site it named is gone. The chosen row has to follow
     the node to whatever tree draws it there rather than vanishing — to the
     node's own row, not to another pointer at it. [add "b" 2 m3] rebuilt the
     spine, so the "d" node the pointer named was allocated here, in the
     structure at the bottom: that is the row wearing the address. *)
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
    (birth 5)
     HEAP                                  2 live · 6 nodes · 3 new
     ▾ m  int M.t  5 bindings
     ├─   "f" → 6
     ├─   "d" → 4
     ├─   "b" → 2
     ├─   "h" → 8
     └─   "j" → 10
     ▾ bigger  int M.t  5 bindings  new
     ├─   "f" → 6  new                               0x78de5b5fff78
     ├─   ↗ "d" → 4
     ├─   "h" → 8  new
     ├─   "g" → 7  new
     └─   ↗ "j" → 10
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
       ~cursor);
  [%expect
    {|
    (cursor (1))
     CALL STACK                            5 calls · 2 live
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
       ~cursor:None);
  [%expect
    {|
    CALL STACK                     5 calls · 2 live · heat
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
