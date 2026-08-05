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

(* The picture tests render through [Cap.dumb], which drops color — so
   anything about fading has to be read back off the escape codes. Renders
   with the ANSI cap, then prints each row beside the theme roles its cells
   are wearing, left to right. *)
let print_palette ?(width = 56) ?(height = 26) view =
  let image = Bonsai_term.View.Private.notty_image view in
  let buffer = Buffer.create 4096 in
  Notty.Render.to_buffer buffer Notty.Cap.ansi (0, 0) (width, height) image;
  let rendered = Buffer.contents buffer in
  (* [\027[38;2;R;G;Bm] sets a 24-bit foreground; [\027[0m] clears it. Every
     other escape (background, bold, cursor moves) only has to be skipped. *)
  let rows = ref [] in
  let row = Buffer.create 128 in
  let roles = ref [] in
  let fg = ref None in
  let note () =
    match !fg with
    | None -> ()
    | Some color ->
      let name = Theme.For_testing.color_name color in
      (match !roles with
       | last :: _ when String.equal last name -> ()
       | _ :: _ | [] -> roles := name :: !roles)
  in
  let end_row () =
    let text = String.rstrip (Buffer.contents row) in
    (match String.is_empty text with
     | true -> ()
     | false -> rows := (text, List.rev !roles) :: !rows);
    Buffer.clear row;
    roles := []
  in
  let n = String.length rendered in
  let i = ref 0 in
  while !i < n do
    match rendered.[!i] with
    | '\027' ->
      let stop = ref (!i + 1) in
      while !stop < n && not (Char.equal rendered.[!stop] 'm') do
        Int.incr stop
      done;
      let params = String.slice rendered (!i + 2) (Int.min !stop n) in
      (* one SGR escape can carry several settings, e.g. [0;38;2;r;g;b] —
         walk them rather than matching the whole list *)
      let rec settings = function
        | "38" :: "2" :: r :: g :: b :: rest ->
          fg
          := Some
               (Bonsai_term.Attr.Color.rgb
                  ~r:(Int.of_string r)
                  ~g:(Int.of_string g)
                  ~b:(Int.of_string b));
          settings rest
        | "48" :: "2" :: _ :: _ :: _ :: rest -> settings rest
        | ("0" | "") :: rest ->
          fg := None;
          settings rest
        | (_ : string) :: rest -> settings rest
        | [] -> ()
      in
      settings (String.split params ~on:';');
      i := !stop + 1
    | '\n' ->
      end_row ();
      Int.incr i
    | char ->
      (match Char.is_whitespace char with true -> () | false -> note ());
      Buffer.add_char row char;
      Int.incr i
  done;
  end_row ();
  List.rev !rows
  |> List.iter ~f:(fun (text, roles) ->
    print_endline
      [%string "%{text#String}   [%{String.concat roles ~sep:\" \"}]"])
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

(* the pane as a view rather than a printout, for [print_palette] — the
   colors are what these tests read, so they cannot go through the dumb cap *)
let heap_image
  ~width
  ~height
  ?(scroll = 0)
  ?(selection = Heap_pane.Selection.none)
  ?folds
  replay
  ~step
  =
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  Heap_pane.view
    ~note:None
    ~total:None
    ~width
    ~height
    ~structures
    ~nodes
    ~new_addresses
    ~folds:(Option.value folds ~default:(Set.empty (module Heap_pane.Fold)))
    ~scroll
    ~selection
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
  print_view
    ~width
    ~height
    (heap_image ~width ~height ~scroll ~selection ?folds replay ~step)
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

(* most stack tests predate the registration tags; an all-[None] array keeps
   their pictures about what they were about *)
let no_registrations calls = Array.map calls ~f:(fun (_ : Call.t) -> None)

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
    ~height:12
    (Stack_pane.view
       ~width:56
       ~height:12
       ~calls:(calls_of replay)
       ~heat:(no_heat (calls_of replay))
       ~live:(live_of replay ~step:2)
       ~selected:1
       ~folds:Int.Set.empty
       ~cursor:None
       ~expanded:Int.Set.empty
       ~registered:(no_registrations (calls_of replay))
       ~scroll:0
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

let%expect_test "heap pane: a faded structure is faded throughout" =
  (* the same three [m]s, read back with their colors: the two shadowed
     structures draw every column in the dim set — guides and glyph down at
     the hairline grays, name and values at ghost, the stats (and the note
     itself) at border — while the live one keeps white-bold names, the ident
     blue on values, and its usual chrome *)
  let replay = replay_of_fixture "map_basic" in
  print_palette ~height:12 (heap_image ~width:56 ~height:12 replay ~step:2);
  [%expect
    {|
    HEAP                          3 live · 4 nodes · 224 B   [secondary faint]
    ▾ m  int M.t  1 binding  1 node · 56 B · shadowed   [border ghost border]
    └─   "a" → 1   [hairline ghost hairline ghost]
    ▾ m  int M.t  2 bindings  2 nodes · 112 B · shadowed   [border ghost border]
    ├─   "a" → 1   [hairline ghost hairline ghost]
    └─   "b" → 2   [hairline ghost hairline ghost]
    ▾ m  int M.t  1 binding  1 node · 56 B   [faint highlight_deep type_name muted faint]
    └─   "b" → 2   [border text ghost value_text]
       name  type  value  new  ↗ shared  faded=unreachable   [text type_name value_text fresh ghost border]
    |}]
;;

let%expect_test "heap pane: rebinding a name fades the versions it left" =
  (* map_basic's last step is three live maps all called [m] — the whole
     point of the note. Color carries it on a real terminal (the faded ones
     drop to the ghost gray); here the words are what shows, which is also
     what [/] filters on. *)
  let replay = replay_of_fixture "map_basic" in
  heap_view replay ~step:2 ~height:12;
  [%expect
    {|
    HEAP                          3 live · 4 nodes · 224 B
    ▾ m  int M.t  1 binding  1 node · 56 B · shadowed
    └─   "a" → 1

    ▾ m  int M.t  2 bindings  2 nodes · 112 B · shadowed
    ├─   "a" → 1
    └─   "b" → 2

    ▾ m  int M.t  1 binding  1 node · 56 B
    └─   "b" → 2

       name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "the pop-out fades with its structure" =
  (* [Enter] on a shadowed structure: the popped drawing keeps the verdict —
     boxes and values in the faded set instead of the card blue and white —
     and the meta says why in words. The [new] tag alone would keep its
     green: allocation is a fact about the step, not about the name. *)
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  let structure = List.hd_exn structures in
  print_palette
    ~width:64
    ~height:8
    (Heap_pane.Diagram.view
       ~structure
       ~structures
       ~nodes
       ~new_addresses
       ~width:64
       ~height:8
       ~scroll:0
       ~pan:0);
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────┐   [border]
    │ DIAGRAM    m · int M.t · shadowed · 1 node · 56 B · esc back │   [border secondary faint border]
    │                                                              │   [border]
    │                          ┌ m ────┐                           │   [border ghost border]
    │                          │"a" → 1│                           │   [border ghost hairline ghost border]
    │                          └───────┘                           │   [border]
    │                                                              │   [border]
    └──────────────────────────────────────────────────────────────┘   [border]
    |}]
;;

let%expect_test "heap pane: a map's l edge is empty, its r edge walked" =
  let replay = replay_of_fixture "map_basic" in
  heap_view replay ~step:1;
  [%expect
    {|
    HEAP                  2 live · 3 nodes · 168 B · 2 new
    ▾ m  int M.t  1 binding  1 node · 56 B · shadowed
    └─   "a" → 1

    ▾ m  int M.t  2 bindings  2 nodes · 112 B  new
    ├─   "a" → 1  new
    └─   "b" → 2  new







       name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "memory: a structure's row sizes it, the meta sums them" =
  (* the wire's own 64-bit words, priced at 8 bytes each: one binding is the
     AVL node's five words (header + l v d r) plus the key's string block (a
     header and one padded word) — 7 words, 56 B. A floor, not a census:
     undecoded pointers count their slot alone. The meta totals the live
     structures the same way. *)
  let replay = replay_of_fixture "map_basic" in
  heap_view ~height:5 replay ~step:0;
  [%expect
    {|
    HEAP                    1 live · 1 node · 56 B · 1 new
    ▾ m  int M.t  1 binding  1 node · 56 B  new
    └─   "a" → 1  new

       name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "heap pane: a queue chains cells off first/next" =
  let replay = replay_of_fixture "queue_basic" in
  heap_view replay ~step:2;
  [%expect
    {|
    HEAP                  1 live · 3 nodes · 104 B · 1 new
    ▾ q  string Queue.t  length 2  3 nodes · 104 B
    ├─   "x"
    └─   "y"  new










       name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "heap pane: boxed map data becomes a d→ child" =
  let replay = replay_of_fixture "map_data_kinds" in
  heap_view replay ~step:2;
  [%expect
    {|
    HEAP                  3 live · 5 nodes · 312 B · 2 new
    ▾ m  float M.t  1 binding  1 node · 72 B
    └─   "pi" → 3.14

    ▾ #2  float M.t  2 bindings  2 nodes · 144 B
    ├─   "pi" → 3.14
    └─   "e" → 2.71

    ▾ #4  (int * string) M.t  1 element  2 nodes · 96 B
        new
    └─ ▾ "pair" →  new
       └─   d  1, "one"  new


       name  type  value  new  ↗ shared  faded=unreachable
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
    ▾ #1  int M.t  1 binding  1 node · 56 B  new
    └─   "dead" → 0  new




       name  type  value  new  ↗ shared  faded=unreachable
    HEAP                    1 live · 1 node · 56 B · 1 new
    ▾ #2  int M.t  1 binding  1 node · 56 B  new
    └─   "live" → 1  new




       name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "heap pane: a Core map is a record over a tagged tree" =
  (* Core reaches its maps through Base, whose map is a record holding the
     comparator and the tree — not the tree itself, the way the stdlib's is.
     The pane reads that straight off the wire: [tree] is the root's one
     edge, and the nodes below it are Base's own [Leaf]/[Node] shapes. *)
  let replay = replay_of_fixture "core_map_basic" in
  (* two of the rows carry a wrapped [· shadowed] now, so the height buys the
     same content two lines further down *)
  heap_view ~width:60 ~height:14 replay ~step:2;
  [%expect
    {|
    HEAP                      3 live · 8 nodes · 280 B · 3 new
    ▾ m  (string, int) Map.t  1 binding  2 nodes · 56 B ·
        shadowed
    └─   "b" → 2

    ▾ m  (string, int) Map.t  2 bindings  3 nodes · 112 B ·
        shadowed
    ├─   "b" → 2
    └─   "a" → 1

    ▾ m  (string, int) Map.t  3 bindings  3 nodes · 112 B  new
    ├─   "b" → 2  new
    ├─   ↗ "a" → 1
           name  type  value  new  ↗ shared  faded=unreachable
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
    ▾ q  (string, int) Hash_queue.t  4 bindings  9 nodes · 240 B
    ├─   v  "a" → 1
    ├─   v  "b" → 2
    ├─   v  "c" → 3  new
    └─   ↗ null















               name  type  value  new  ↗ shared  faded=unreachable
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
      p  point  x=3  y=4  1 node · 24 B

    ▾ ts  trades  0  1 node · 24 B  new
    └─ ▾ hd  t  trade  101  3 nodes · 112 B
       ├─   tags  "buy", "limit"
       └─   span  1, 9






           name  type  value  new  ↗ shared  faded=unreachable
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
        1 node ·
        24 B

    ▾ ts  trades
        0  1 node
        · 24 B
        new
    └─ ▾ hd  t
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
    │ DIAGRAM             m · int M.t · 2 nodes · 112 B · esc back │
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
    │ DIAGRAM                      bigger · int M.t · 6 nodes · 168 B · esc back │
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
    │ DIAGRAM                    q · int M.t Queue.t · 3 nodes · 48 B · esc back │
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
       ~expanded:Int.Set.empty
       ~registered:(no_registrations (calls_of replay))
       ~scroll:0
       ~collapsed:true);
  let heights ?stack_collapsed ?source_collapsed () =
    let layout =
      Layout.compute
        ?stack_collapsed
        ?source_collapsed
        { Bonsai_term.Dimensions.width = 100; height = 40 }
        ~flame_open:false
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
     ./lib/gone.ml — the dump's paths
     resolve from the replayed
     program's root, so run there or
     pass -source-root DIR
    |}]
;;

let%expect_test "transport: ticks, then the clickable key legend" =
  print_view
    ~width:117
    ~height:3
    (Transport.view
       ~width:117
       ~step:1
       ~total:3
       ~density:[| 0.0; 1.0; 0.2 |]
       ~playing:false
       ~accordion:false
       ~diagram:false
       ~flame:Shut);
  [%expect
    {|
    ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
    ◂ back · step ▸ · [space] play · . latest · ↑↓ node · ⏎ diagram · h fold · z accordion · / filter · f flame · q quit
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
    ▾ a  S.t  3 elements  3 nodes · 96 B
    ├─   1
    ├─   2
    └─   3

    ▾ b  S.t  2 elements  2 nodes · 64 B
    ├─   3
    └─   4

    ▾ #6  S.t  4 elements  4 nodes · 128 B  new
    ├─   3  new
    ├─   2  new
    ├─   1  new
           name  type  value  new  ↗ shared  faded=unreachable
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
     only [y] decides; the blank line between the structures belongs to
     nobody; the second structure's row past it is a place of its own; and
     below the last row there is nothing to land on *)
  print_endline (at ~x:2 ~y:0);
  print_endline (at ~x:48 ~y:0);
  print_endline (at ~x:2 ~y:2);
  print_endline (at ~x:2 ~y:3);
  print_endline (at ~x:2 ~y:9);
  [%expect
    {|
    0x72d2a9feeb50
    0x72d2a9feeb50
    ·
    0x72d2a9fea718
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
    ▾ m  int M.t  1 binding  1 node · 56 B
    └─   "k" → 1

      q  int M.t Queue.t  length 0  1 node · 24 B  new








       name  type  value  new  ↗ shared  faded=unreachable
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
    ▾ q  int M.t Queue.t  length 1  2 nodes · 48 B
    └─ ▾ m  int M.t  1 binding  1 node · 56 B
       └─   "k" → 1
















       name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "heap pane: hashtbl walks record → bucket array → chain" =
  let replay = replay_of_fixture "hashtbl_basic" in
  heap_view ~width:60 ~height:17 replay ~step:1;
  [%expect
    {|
    HEAP                      1 live · 3 nodes · 208 B · 1 new
    ▾ tbl  (string, int) Hashtbl.t  size 1  3 nodes · 208 B
    └─   "a" → 1  new













           name  type  value  new  ↗ shared  faded=unreachable
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
    ▾ qq  'a Queue.t Queue.t  length 1  2 nodes · 48 B
    └─   q1  'a Queue.t  length 0  1 node · 24 B






















           name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "heap pane: closures stay opaque" =
  let replay = replay_of_fixture "queue_of_closures" in
  heap_view ~width:60 ~height:13 replay ~step:1;
  [%expect
    {|
    HEAP                       1 live · 2 nodes · 48 B · 2 new
    ▾ q  (int -> int) Queue.t  length 1  2 nodes · 48 B
    └─   ⟨0x7f8238bebce8⟩  new









           name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "control chips hit-test exactly where they render" =
  (* the row wants 117 columns now that [f flame] rides beyond [/ filter];
     narrower than that and [start_column] pins it at 0 and the right-hand
     chips crop *)
  let width = 117 in
  let show (flame : Transport.Flame_state.t) =
    let hits =
      List.filter_map (List.init width ~f:Fn.id) ~f:(fun x ->
        Transport.control_at ~width ~playing:false ~flame ~x
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
        [%sexp (button : Transport.Button.t), (first : int), (last : int)])
  in
  show Shut;
  (* focusing the drawer swaps the middle of the row, so extents move *)
  show Focused;
  [%expect
    {|
    (Back 0 5)
    (Step 9 14)
    (Play 18 29)
    (Latest 33 40)
    (Node 44 50)
    (Diagram 54 62)
    (Fold 66 71)
    (Accordion 75 85)
    (Filter 89 96)
    (Flame 100 106)
    (Quit 110 115)
    (Back 17 22)
    (Step 26 31)
    (Play 35 46)
    (Latest 50 57)
    (Node 61 66)
    (Zoom 70 75)
    (Reset_zoom 79 85)
    (Filter 89 96)
    (Flame 100 106)
    (Quit 110 115)
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
    HEAP                  2 live · 3 nodes · 104 B · 1 new
    ▾ q  int M.t Queue.t  length 1  2 nodes · 48 B
    └─ ▸ m  int M.t  1 binding  1 node · 56 B  ⋯ 1






       name  type  value  new  ↗ shared  faded=unreachable
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
    HEAP                  2 live · 3 nodes · 104 B · 1 new
    ▸ q  int M.t Queue.t  length 1  2 nodes · 48 B  ⋯ 2





       name  type  value  new  ↗ shared  faded=unreachable
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
     (rows_expanded (0 5 9)) (rows_folded (0 2 6)))
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
  (* #1's own glyph; the binding under it is a leaf, so no glyph; the gap
     between the structures has nothing to toggle; #2's own glyph past it; a
     cell that is not the glyph column; past the last row *)
  print_endline (toggle ~x:0 ~y:0);
  print_endline (toggle ~x:0 ~y:1);
  print_endline (toggle ~x:0 ~y:2);
  print_endline (toggle ~x:0 ~y:3);
  print_endline (toggle ~x:3 ~y:3);
  print_endline (toggle ~x:0 ~y:9);
  [%expect
    {|
    (Structure 1)
    ·
    ·
    (Structure 2)
    ·
    ·
    |}]
;;

let%expect_test "stack fold: a call's range tucks behind a count" =
  let replay = replay_of_fixture "map_fold" in
  print_view
    ~height:9
    (Stack_pane.view
       ~width:56
       ~height:9
       ~calls:(calls_of replay)
       ~heat:(no_heat (calls_of replay))
       ~live:(live_of replay ~step:4)
       ~selected:0
       ~folds:(Int.Set.of_list [ 1 ])
       ~cursor:None
       ~expanded:Int.Set.empty
       ~registered:(no_registrations (calls_of replay))
       ~scroll:0
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
    HEAP                      3 live · 9 nodes · 288 B · 4 new
    ▾ a  S.t  3 elements  3 nodes · 96 B
    ├─   1
    ├─   2
    └─   3

    ▾ b  S.t  2 elements  2 nodes · 64 B
    ├─   3
    └─   4

    ▾ #6  S.t  4 elements  4 nodes · 128 B  new
    ├─   3  new
    ├─   2  new
           name  type  value  new  ↗ shared  faded=unreachable
    HEAP                      3 live · 9 nodes · 288 B · 4 new
    ▾ a  S.t  3 elements  3 nodes · 96 B
    ├─   1
    ├─   2
    └─   3

    ▸ b  S.t  2 elements  2 nodes · 64 B  ⋯ 2

    ▾ #6  S.t  4 elements  4 nodes · 128 B  new
    ├─   3  new
    ├─   2  new
    ├─   1  new
    └─   4  new
           name  type  value  new  ↗ shared  faded=unreachable
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
    HEAP                  2 live · 3 nodes · 168 B · 2 new
    ▾ #1  int M.t  1 binding  1 node · 56 B
    └─   "a" → 1

    ▾ m  int M.t  2 bindings  2 nodes · 112 B  new
    ├─   "a" → 1  new
    └─   "b" → 2  new

       name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "delta wire: a shared payload is drawn once, then pointed at"
  =
  let replay = replay_of_fixture "map_shared_payload" in
  heap_view ~width:64 ~height:30 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                          4 live · 7 nodes · 360 B · 3 new
    ▾ #2  point M.t  1 element  1 node · 56 B
    └─ ▾ "p" →
       └─   d  p  point  x=1  y=2  1 node · 24 B

    ▾ m  point M.t  2 elements  2 nodes · 112 B
    ├─ ▾ "p" →
    │  └─   d  ↗ x=1  y=2
    └─ ▾ "q" →
       └─   d  ↗ x=1  y=2

    ▾ #5  point M.t  3 elements  3 nodes · 168 B  new
    ├─ ▾ "p" →  new
    │  └─   d  ↗ x=1  y=2
    ├─ ▾ "q" →  new
    │  └─   d  ↗ x=1  y=2
    └─ ▾ "r" →  new
       └─   d  ↗ x=1  y=2











               name  type  value  new  ↗ shared  faded=unreachable
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
    ▾ m  int M.t  5 bindings  3 nodes · 168 B
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10

    ▾ bigger  int M.t  5 bindings  3 nodes · 168 B  new
    ├─   "f" → 6  new
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
    ├─   "g" → 7  new
    └─   ↗ "j" → 10





















               name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "delta wire: a payload cycle terminates" =
  let replay = replay_of_fixture "queue_cycle" in
  heap_view ~width:64 ~height:30 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                           2 live · 3 nodes · 88 B · 1 new
    ▾ q  cyc Queue.t  length 1  2 nodes · 48 B
    └─   r  cyc  name="loop"  self=0  1 node · 40 B


























               name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "delta wire: version chains share their spines" =
  let replay = replay_of_fixture "map_versions" in
  heap_view ~width:64 ~height:20 replay ~step:(Replay.length replay - 1);
  [%expect
    {|
    HEAP                          2 live · 8 nodes · 448 B · 4 new
    ▾ #7  int M.t  4 bindings  4 nodes · 224 B
    ├─   "b" → 2
    ├─   "a" → 1
    ├─   "c" → 3
    └─   "d" → 4

    ▾ #11  int M.t  5 bindings  4 nodes · 224 B  new
    ├─   "b" → 2  new
    ├─   ↗ "a" → 1
    ├─   "c" → 3  new
    ├─   "d" → 4  new
    └─   "e" → 5  new






               name  type  value  new  ↗ shared  faded=unreachable
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
    HEAP                                      2 live · 6 nodes · 336 B · 3 new
    ▾ m  int M.t  5 bindings  3 nodes · 168 B
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10

    ▾ bigger  int M.t  5 bindings  3 nodes · 168 B  new         0x7334179fff78
    ├─   "f" → 6  new                                           0x7334179fff78
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
    ├─   "g" → 7  new
    └─   ↗ "j" → 10











                           name  type  value  new  ↗ shared  faded=unreachable
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

let%expect_test "[h] on a row a fold hid resolves to what still draws it" =
  (* The cursor can be standing on a row inside a structure when the whole
     structure folds up — the fallback after stepping does exactly that. [h]
     resolves the spot against the outline before folding: the hidden row's
     node is now drawn only by the structure's own row (they share the root's
     address), whose fold is the structure itself, so [h] reopens it instead
     of flipping a node fold nobody can see. *)
  let replay = replay_of_fixture "map_basic" in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  let root =
    List.find_exn structures ~f:(fun (s : Replay.Structure.t) ->
      s.is_current)
    |> Heap_pane.spot_of_structure
  in
  (* drop off the header onto the root binding's own row *)
  let inside =
    Heap_pane.move_cursor
      ~structures
      ~nodes
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~selection:{ Heap_pane.Selection.selected = Some root; cursor = None }
      ~direction:Heap_pane.Direction.Right
    |> Option.value_exn
  in
  print_s [%sexp (Heap_pane.fold_of_spot inside : Heap_pane.Fold.t)];
  let resolved =
    Heap_pane.resolve_spot
      ~structures
      ~nodes
      ~new_addresses
      ~folds:
        (Set.of_list (module Heap_pane.Fold) [ Heap_pane.fold_of_spot root ])
      inside
    |> Option.value_exn
  in
  print_s [%sexp (Heap_pane.fold_of_spot resolved : Heap_pane.Fold.t)];
  [%expect {|
    (Node 2 ())
    (Structure 2)
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
    HEAP          accordion · 2 live · 6 nodes · 336 B · 3 new
    ▸ m  int M.t  5 bindings  3 nodes · 168 B  ⋯ 5

    ▾ bigger  int M.t  5 bindings  3 nodes · 168 B  new
                                                0x7334179fff78
    ├─   "f" → 6  new
    ├─   ↗ "d" → 4
    ├─   "h" → 8  new
    ├─   "g" → 7  new
    └─   ↗ "j" → 10

           name  type  value  new  ↗ shared  faded=unreachable
    |}];
  (* [w] to the open structure's header, [w] onto the one before it: the
     arrival opens [m], and [bigger] folds up behind us *)
  move Heap_pane.Direction.Up;
  move Heap_pane.Direction.Up;
  show !selection;
  [%expect
    {|
    HEAP          accordion · 2 live · 6 nodes · 336 B · 3 new
    ▾ m  int M.t  5 bindings  3 nodes · 168 B   0x733427af6a08
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10

    ▸ bigger  int M.t  5 bindings  3 nodes · 168 B  ⋯ 5  new
                                                0x7334179fff78

           name  type  value  new  ↗ shared  faded=unreachable
    |}]
;;

let%expect_test "a step lands the heap on the structure it walked" =
  (* Stepping resets the scroll — but to the selection's row, roughly
     centered, not to the top of the canvas: on a dump with hundreds of
     structures the walked one can sit far below the fold, and a pane that
     opened on the top showed everything except the thing the step was about.
     [bigger] is the walked structure here and sits below [m]'s six lines, so
     the landing is a real scroll, inside the outline — and the render at it
     has [bigger]'s row on screen. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let selection =
    { Heap_pane.Selection.selected = None
    ; cursor = current_spot replay ~step
    }
  in
  let scroll =
    Heap_pane.landing
      ~structures
      ~nodes
      ~new_addresses
      ~folds:(Set.empty (module Heap_pane.Fold))
      ~selection
      ~width:44
      ~height:8
  in
  printf "scroll=%d\n" scroll;
  heap_view ~width:44 ~height:8 ~scroll ~selection replay ~step;
  [%expect
    {|
    scroll=4
     HEAP      2 live · 6 nodes · 336 B · 3 new
     ├─   "h" → 8
     └─   "j" → 10

     ▾ bigger  int M.t  5 bindings  3 nodes ·
         168 B  new              0x7334179fff78
     ├─   "f" → 6  new
    |}]
;;

let%expect_test "[o]: address order packs allocation neighbors together" =
  (* Registry order is creation order — [m] before [bigger] — but the
     [Gc.full_major] between them moved [m], so ascending addresses put
     [bigger] first. The sort is applied by the app's [heap_inputs] (the [o]
     toggle), top level only; the pane draws whatever order it is given. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  let step = Replay.length replay - 1 in
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  let order structures =
    List.map structures ~f:(fun (s : Replay.Structure.t) ->
      [%string
        "%{Replay.Structure.display s} %{Snapshot.Address.display s.address}"])
  in
  print_s [%sexp (order structures : string list)];
  print_s [%sexp (order (Heap_pane.by_address structures) : string list)];
  print_view
    ~width:44
    ~height:5
    (Heap_pane.view
       ~note:(Some "by address")
       ~total:None
       ~width:44
       ~height:5
       ~structures:(Heap_pane.by_address structures)
       ~nodes
       ~new_addresses
       ~folds:
         (Set.of_list
            (module Heap_pane.Fold)
            (List.map structures ~f:(fun (s : Replay.Structure.t) ->
               Heap_pane.Fold.Structure s.id)))
       ~scroll:0
       ~selection:Heap_pane.Selection.none);
  [%expect
    {|
    ("m 0x733427af6a08" "bigger 0x7334179fff78")
    ("bigger 0x7334179fff78" "m 0x733427af6a08")
     HEAP by address · 2 live · 6 nodes · 336 B
     ▸ bigger  int M.t  6 bindings  3 nodes ·
         168 B  ⋯ 6  new
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
    HEAP 2 live · 6 nodes · 336 B · 3
    ▾ m  int M.t  5 bindings  3
        nodes · 168 B
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10

    ▾ bigger  int M.t  5 bindings  3
        nodes · 168 B  new
    HEAP 2 live · 6 nodes · 336 B · 3
        nodes · 168 B
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10

    ▾ bigger  int M.t  5 bindings  3
        nodes · 168 B  new
                      0x7334179fff78
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
    HEAP      /queue · 1 of 2 live · 1 node · 24 B · 1 new
      q  int M.t Queue.t  length 0  1 node · 24 B  new



       name  type  value  new  ↗ shared  faded=unreachable
    |}];
  (* matching is case-insensitive, and the kind is part of the header's
     words, so [/MAP] finds the map *)
  show "MAP";
  [%expect
    {|
    HEAP        /MAP · 1 of 2 live · 1 node · 56 B · 1 new
    ▾ m  int M.t  1 binding  1 node · 56 B
    └─   "k" → 1


       name  type  value  new  ↗ shared  faded=unreachable
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
     HEAP                          2 live · 6 nodes · 336 B · 3 new
     ├─   "f" → 6
     ├─   "d" → 4
     ├─   "b" → 2
     ├─   "h" → 8
     └─   "j" → 10

     ▾ bigger  int M.t  5 bindings  3 nodes · 168 B  new
     ├─   "f" → 6  new                               0x7334179fff78
     ├─   ↗ "d" → 4
     ├─   "h" → 8  new
     ├─   "g" → 7  new
     └─   ↗ "j" → 10
                name  type  value  new  ↗ shared  faded=unreachable
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
      ~expanded:Int.Set.empty
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
       ~expanded:Int.Set.empty
       ~registered:(no_registrations (calls_of replay))
       ~scroll:0
       ~collapsed:false);
  [%expect
    {|
    (cursor (1))
     ▾ CALL STACK                          5 calls · 2 live
          M.add "b" 2 M.empty

     ▎▾ M.add "a" 1 (M.add "b" 2 M.empty)

     ▎    M.add k (v * 2) acc

          M.add k (v * 2) acc
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
       ~expanded:Int.Set.empty
       ~registered:(no_registrations (calls_of replay))
       ~scroll:0
       ~collapsed:false);
  [%expect
    {|
    ▾ CALL STACK                   5 calls · 2 live · heat
         M.add "b" 2 M.empty

     ▾ M.add "a" 1 (M.add "b" 2 M.empty)

    ▎    M.add k (v * 2) acc

         M.add k (v * 2) acc

     ▾ M.fold (fun k v acc -> M.add k (v * 2) acc) m
    |}]
;;

let%expect_test "session bar: the heat legend names what the ramp measures" =
  print_view
    ~width:80
    ~height:1
    (Session_bar.view
       ~width:80
       ~dump_name:"greet.dump"
       ~structure:"Map"
       ~heat:(Some `Compute));
  [%expect
    {| ● ocaml-debug │ greet.dump │ Map · replay                   heat █████ compute |}];
  print_view
    ~width:80
    ~height:1
    (Session_bar.view
       ~width:80
       ~dump_name:"greet.dump"
       ~structure:"Map"
       ~heat:(Some `Calls));
  [%expect
    {| ● ocaml-debug │ greet.dump │ Map · replay                     heat █████ calls |}];
  print_view
    ~width:80
    ~height:1
    (Session_bar.view
       ~width:80
       ~dump_name:"greet.dump"
       ~structure:"Map"
       ~heat:None);
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

(* a synthetic stack for the repeat-run behavior: golden dumps have no 4-long
   same-function runs (the suite above proving exactly that), so build one —
   six Queue.add leaves between two distinct calls *)
let run_heavy_stack () =
  let info index name : Call.Info.t =
    { depth = 1
    ; id = index
    ; function_info = Function_info.Function_name name
    ; location =
        Location.create
          ~file_path:"t.ml"
          ~line_number:(index + 1)
          ~char_range:(0, 1)
    ; arguments = []
    ; registry = []
    ; ty = None
    ; binder = None
    ; scope = None
    ; snapshot = Snapshot.empty
    }
  in
  let parsed_info = Queue.create () in
  List.iteri
    ([ "start" ]
     @ List.init 6 ~f:(fun (_ : int) -> "Queue.add")
     @ [ "finish" ])
    ~f:(fun index name -> Queue.enqueue parsed_info (info index name));
  Call_stack.create ~parsed_info
;;

let%expect_test "stack pane: a repeat run collapses behind ×N" =
  let calls = (run_heavy_stack ()).call_order in
  print_view
    ~height:6
    (Stack_pane.view
       ~width:56
       ~height:6
       ~calls
       ~heat:(no_heat calls)
       ~live:[ 0 ]
       ~selected:0
       ~folds:Int.Set.empty
       ~cursor:None
       ~expanded:Int.Set.empty
       ~registered:(no_registrations calls)
       ~scroll:0
       ~collapsed:false);
  [%expect
    {|
    ▾ CALL STACK                          8 calls · 1 live
    ▎  start

     ▸ Queue.add  ⋯ ×6

       finish
    |}]
;;

let%expect_test "stack pane: an expanded run shows every repeat" =
  let calls = (run_heavy_stack ()).call_order in
  print_view
    ~height:16
    (Stack_pane.view
       ~width:56
       ~height:16
       ~calls
       ~heat:(no_heat calls)
       ~live:[ 0 ]
       ~selected:0
       ~folds:Int.Set.empty
       ~cursor:None
       ~expanded:(Int.Set.of_list [ 1 ])
       ~registered:(no_registrations calls)
       ~scroll:0
       ~collapsed:false);
  [%expect
    {|
    ▾ CALL STACK                          8 calls · 1 live
    ▎  start

     ▾ Queue.add

       Queue.add

       Queue.add

       Queue.add

       Queue.add

       Queue.add

       finish
    |}]
;;

let%expect_test "stack pane: a run holding the selection never collapses" =
  let calls = (run_heavy_stack ()).call_order in
  print_view
    ~height:16
    (Stack_pane.view
       ~width:56
       ~height:16
       ~calls
       ~heat:(no_heat calls)
       ~live:[ 3 ]
       ~selected:0
       ~folds:Int.Set.empty
       ~cursor:None
       ~expanded:Int.Set.empty
       ~registered:(no_registrations calls)
       ~scroll:0
       ~collapsed:false);
  [%expect
    {|
    ▾ CALL STACK                          8 calls · 1 live
       start

       Queue.add

       Queue.add

    ▎  Queue.add

       Queue.add

       Queue.add

       Queue.add

       finish
    |}]
;;

let%expect_test "cursor walks a collapsed run as one row" =
  let calls = (run_heavy_stack ()).call_order in
  let step cursor =
    Stack_pane.move_cursor
      ~calls
      ~live:[ 0 ]
      ~selected:0
      ~folds:Int.Set.empty
      ~cursor
      ~expanded:Int.Set.empty
      ~direction:`Down
  in
  let first = step None in
  let second = Option.bind first ~f:(fun c -> step (Some c)) in
  print_s [%message (first : int option) (second : int option)];
  [%expect {| ((first (1)) (second (7))) |}]
;;

let%expect_test "timeline density brightens within each state's hue" =
  List.iter [ 0.0; 0.2; 0.7 ] ~f:(fun density ->
    let index ramp color =
      fst
        (Array.findi_exn ramp ~f:(fun (_ : int) stop ->
           phys_equal stop color))
    in
    let past =
      index
        Theme.tick_past_ramp
        (Theme.tick_density Theme.tick_past_ramp ~density)
    in
    let future =
      index
        Theme.tick_future_ramp
        (Theme.tick_density Theme.tick_future_ramp ~density)
    in
    print_endline
      [%string "%{density#Float} -> past %{past#Int}, future %{future#Int}"]);
  [%expect
    {|
    0. -> past 0, future 0
    0.2 -> past 1, future 1
    0.7 -> past 2, future 2
    |}]
;;

(* ── the flame panel ────────────────────────────────────────────────── *)

(* golden dumps top out at eight calls and three deep, which cannot exercise
   pooling or depth scrolling, so shapes here are built the way
   [run_heavy_stack] builds its run: [Call.Info.t] records straight into a
   queue, in the order the wire writes them — children before parents *)
let flame_info ~depth ~name : Call.Info.t =
  { depth
  ; id = depth
  ; function_info = Function_info.Function_name name
  ; location =
      Location.create ~file_path:"t.ml" ~line_number:depth ~char_range:(0, 1)
  ; arguments = []
  ; registry = []
  ; ty = None
  ; binder = None
  ; scope = None
  ; snapshot = Snapshot.empty
  }
;;

let flame_stack frames =
  let parsed_info =
    Queue.of_list
      (List.map frames ~f:(fun (name, depth) -> flame_info ~depth ~name))
  in
  Call_stack.create ~parsed_info
;;

let flame_of ?profile frames =
  Flame_tree.create ~calls:(flame_stack frames).call_order ~profile
;;

(* main over three callees, one of them called twice and calling twice
   itself: enough shape for tiling, a self run, and a path to light *)
let sample_frames =
  [ "read", 3
  ; "read", 3
  ; "lex", 2
  ; "lex", 2
  ; "parse", 2
  ; "emit", 2
  ; "main", 1
  ]
;;

let flame_view
  ?(width = 60)
  ?(height = 8)
  ?(zoom = [])
  ?cursor
  ?(depth_scroll = 0)
  ?(live = [])
  tree
  =
  Flame_pane.view
    ~width
    ~height
    ~open_:true
    ~tree
    ~live
    ~zoom
    ~cursor
    ~depth_scroll
;;

let%expect_test "flame: a row's columns sum to its width exactly" =
  let show ~width ~self ~children =
    let cells = Flame_pane.columns ~width ~self ~children in
    let total =
      List.sum (module Int) cells.children ~f:Fn.id + cells.pool + cells.self
    in
    print_s
      [%message
        (width : int) (cells : Flame_pane.Columns.t) ~sums_to:(total : int)]
  in
  (* the worked example from the mli, so the documented arithmetic is
     executable rather than aspirational *)
  show ~width:40 ~self:4 ~children:[ 7; 5; 3; 1 ];
  show ~width:3 ~self:1 ~children:[ 7; 5; 3; 1 ];
  (* every child keeps a column while there is room for one *)
  show ~width:4 ~self:1 ~children:[ 7; 5; 3; 1 ];
  (* degenerate widths must not over- or under-fill the row *)
  show ~width:1 ~self:1 ~children:[ 7; 5 ];
  show ~width:2 ~self:0 ~children:[ 1 ];
  show ~width:0 ~self:1 ~children:[ 1 ];
  show ~width:10 ~self:0 ~children:[];
  [%expect
    {|
    ((width 40) (cells ((children (14 10 6 3)) (pool 0) (pooled 0) (self 7)))
     (sums_to 40))
    ((width 3) (cells ((children (1 1)) (pool 1) (pooled 2) (self 0)))
     (sums_to 3))
    ((width 4) (cells ((children (1 1 1 1)) (pool 0) (pooled 0) (self 0)))
     (sums_to 4))
    ((width 1) (cells ((children ()) (pool 1) (pooled 2) (self 0))) (sums_to 1))
    ((width 2) (cells ((children (2)) (pool 0) (pooled 0) (self 0))) (sums_to 2))
    ((width 0) (cells ((children ()) (pool 0) (pooled 0) (self 0))) (sums_to 0))
    ((width 10) (cells ((children ()) (pool 0) (pooled 0) (self 0))) (sums_to 0))
    |}]
;;

let%expect_test "flame: the flames rise, roots on the bottom row" =
  print_view ~width:60 ~height:8 (flame_view (flame_of sample_frames));
  [%expect
    {|
    ▾ FLAME           7 events · width = calls · color = calls




     read
     emit     lex                             parse
     main
    |}]
;;

let%expect_test "flame: every row tiles the pane exactly" =
  (* boxes are FILLED now, so a colourless render cannot see where one ends
     and the next begins — the tiling has to be asserted where it is decided
     instead. [columns] is checked directly above; here every row of a real
     tree goes through the same apportionment, and the widths are summed. *)
  let tree = flame_of sample_frames in
  let width = 58 in
  let rec walk (node : Flame_tree.Node.t) =
    match node.children with
    | [] -> ()
    | children ->
      let cells =
        Flame_pane.columns
          ~width
          ~self:node.calls
          ~children:
            (List.map children ~f:(fun (child : Flame_tree.Node.t) ->
               child.inclusive))
      in
      let total =
        List.sum (module Int) cells.children ~f:Fn.id
        + cells.pool
        + cells.self
      in
      let name = Flame_tree.Key.display node.key in
      print_s [%message name (total : int) ~of_:(width : int)];
      List.iter children ~f:walk
  in
  List.iter tree.roots ~f:walk;
  [%expect
    {|
    (main (total 58) (of_ 58))
    (lex (total 58) (of_ 58))
    |}]
;;

let%expect_test "flame: a click lands on the box the eye is over" =
  (* and the boundaries the hit-test reports ARE the drawn boundaries, since
     [bar_at] and [view] share one apportionment — so walking a row column by
     column is the picture's tiling, read back out *)
  let tree = flame_of sample_frames in
  let at ~x ~row =
    Flame_pane.bar_at
      ~width:60
      ~height:8
      ~tree
      ~live:[]
      ~zoom:[]
      ~cursor:None
      ~depth_scroll:0
      ~x
      ~row
  in
  (* the body is 7 rows and the tree is 3 deep, so bottom-alignment puts the
     roots on row 6 and their callees on row 5 — the row with several boxes
     on it, and the one worth walking column by column *)
  List.init 58 ~f:(fun x -> x, at ~x ~row:5)
  |> List.group ~break:(fun ((_ : int), a) ((_ : int), b) ->
    not ([%equal: Flame_pane.Path.t option] a b))
  |> List.iter ~f:(fun group ->
    let first, path = List.hd_exn group in
    let last, (_ : Flame_pane.Path.t option) = List.last_exn group in
    let name =
      match path with
      | None -> "-"
      | Some path ->
        List.map path ~f:Flame_tree.Key.display |> String.concat ~sep:">"
    in
    print_s [%message name (first : int) (last : int)]);
  [%expect
    {|
    (main>emit (first 0) (last 8))
    (main>lex (first 9) (last 40))
    (main>parse (first 41) (last 49))
    (- (first 50) (last 57))
    |}]
;;

let%expect_test "flame: children too narrow to draw pool behind +N" =
  let wide =
    List.init 8 ~f:(fun index -> [%string "f%{index#Int}"], 2)
    @ [ "main", 1 ]
  in
  print_view
    ~width:8
    ~height:5
    (flame_view ~width:8 ~height:5 (flame_of wide));
  (* wider, where the [+N] has room to say how many it stands for *)
  print_view
    ~width:16
    ~height:5
    (flame_view ~width:16 ~height:5 (flame_of wide));
  let cells =
    Flame_pane.columns
      ~width:6
      ~self:1
      ~children:(List.init 8 ~f:(fun (_ : int) -> 1))
  in
  print_s [%sexp (cells : Flame_pane.Columns.t)];
  [%expect
    {|
     ▾ FLAME


     fff+5
      main
     ▾ FLAME 9 event


     f0f1f2f3f4f5ff
      main
    ((children (1 1 1)) (pool 3) (pooled 5) (self 0))
    |}]
;;

let%expect_test "flame: the lit path tracks the step" =
  let stack = flame_stack sample_frames in
  let tree = Flame_tree.create ~calls:stack.call_order ~profile:None in
  List.iter [ 0; 4; 6 ] ~f:(fun step ->
    let live =
      Flame_pane.live_path tree ~frames:(Call_stack.frames_at stack ~step)
    in
    print_endline [%string "step %{step#Int}:"];
    print_view ~width:60 ~height:5 (flame_view ~height:5 ~live tree));
  [%expect
    {|
    step 0:
     ▾ FLAME           7 events · width = calls · color = calls

     ▏ read
      emit    ▏ lex                            parse
     ▏ main
    step 4:
     ▾ FLAME           7 events · width = calls · color = calls

      read
      emit     lex                            ▏ parse
     ▏ main
    step 6:
     ▾ FLAME           7 events · width = calls · color = calls

      read
      emit     lex                             parse
     ▏ main
    |}]
;;

let%expect_test "flame: a narrow bar keeps what fits of its name" =
  List.iter [ 2; 3; 4; 6; 12 ] ~f:(fun width ->
    print_view
      ~width
      ~height:3
      (flame_view ~width ~height:3 (flame_of [ "tokenize", 2; "main", 1 ])));
  [%expect
    {|
    ▾
    ▾
    t
    m
    ▾ F
    to
    ma
    ▾ FLA
    to
    main
    ▾ FLAME 2 e
    toke⋯
     main
    |}]
;;

let%expect_test "flame: zoom rescales the subtree to the whole pane" =
  let tree = flame_of sample_frames in
  print_view
    ~width:60
    ~height:5
    (flame_view ~height:5 ~zoom:[ Named "main"; Named "lex" ] tree);
  [%expect
    {|
    ▾ FLAME   ⌖ lex · 7 events · width = calls · color = calls


     read
     lex
    |}]
;;

let%expect_test "flame: the cursor walks the tree, up into the callees" =
  let tree = flame_of sample_frames in
  let step cursor direction =
    Flame_pane.move_cursor
      ~width:60
      ~tree
      ~zoom:[]
      ~cursor
      ~live:[]
      ~direction
  in
  let show cursor =
    print_s
      [%sexp
        (Option.map cursor ~f:(List.map ~f:Flame_tree.Key.display)
         : string list option)]
  in
  (* nothing aimed at yet: the first press lands somewhere sensible *)
  let start = step None Up in
  show start;
  (* [Down] from a root goes nowhere: it IS the bottom row *)
  show (step start Down);
  (* [Up] climbs into the widest callee, which under name ordering is not the
     first one drawn *)
  let deeper = step start Up in
  show deeper;
  (* and [Down] from there comes back to the caller *)
  show (step deeper Down);
  (* [Right] runs along the callees; off the end the cursor stays put *)
  let right = step deeper Right in
  show right;
  show (step right Right);
  [%expect
    {|
    ((main))
    ()
    ((main lex))
    ((main))
    ((main parse))
    ()
    |}]
;;

let%expect_test "flame: a deep tree scrolls by depth" =
  let deep =
    List.init 20 ~f:(fun index -> [%string "d%{index#Int}"], 20 - index)
  in
  let tree = flame_of deep in
  print_view ~width:40 ~height:6 (flame_view ~width:40 ~height:6 tree);
  print_view
    ~width:40
    ~height:6
    (flame_view ~width:40 ~height:6 ~depth_scroll:8 tree);
  [%expect
    {|
    ▾ FLAME depth 0-4 of 20 · 20 events · w
     d15
     d16
     d17
     d18
     d19
    ▾ FLAME depth 8-12 of 20 · 20 events ·
     d7
     d8
     d9
     d10
     d11
    |}]
;;

let%expect_test "flame: a profile changes the colors, not the layout" =
  let profile =
    Heat_profile.t_of_sexp
      (Sexp.of_string
         {|
((version 1) (root_module Main)
 (entries
  (((module_path (Main)) (kind (Named lex)) (samples 900))
   ((module_path (Main)) (kind (Named parse)) (samples 100)))))
|})
  in
  (* the picture is identical either way — only the meta line and the colors
     change, and color is stripped here *)
  print_view
    ~width:60
    ~height:5
    (flame_view ~height:5 (flame_of sample_frames));
  print_view
    ~width:60
    ~height:5
    (flame_view ~height:5 (flame_of ~profile sample_frames));
  [%expect
    {|
    ▾ FLAME           7 events · width = calls · color = calls

     read
     emit     lex                             parse
     main
    ▾ FLAME         7 events · width = calls · color = compute

     read
     emit     lex                             parse
     main
    |}]
;;

let%expect_test "flame: shut, the drawer is its title row and nothing else" =
  let tree = flame_of sample_frames in
  (* [Layout] gives a shut drawer exactly [collapsed_flame_height] rows, so
     the body falls away on its own — the [▸] is the only thing that has to
     change, and the meta keeps saying what is behind it *)
  print_view
    ~width:60
    ~height:Layout.collapsed_flame_height
    (Flame_pane.view
       ~width:60
       ~height:Layout.collapsed_flame_height
       ~open_:false
       ~tree
       ~live:[]
       ~zoom:[]
       ~cursor:None
       ~depth_scroll:0);
  (* and nothing in it can be clicked: the app reads a click on the title row
     as "open me" precisely because [bar_at] declines it *)
  print_s
    [%sexp
      (Flame_pane.bar_at
         ~width:60
         ~height:Layout.collapsed_flame_height
         ~tree
         ~live:[]
         ~zoom:[]
         ~cursor:None
         ~depth_scroll:0
         ~x:4
         ~row:0
       : Flame_pane.Path.t option)];
  [%expect
    {|
     ▸ FLAME           7 events · width = calls · color = calls
    ()
    |}]
;;

let%expect_test "layout: opening the drawer takes rows from the heap" =
  let dimensions = { Bonsai_term.Dimensions.width = 80; height = 24 } in
  List.iter [ false; true ] ~f:(fun flame_open ->
    print_s [%sexp (Layout.compute dimensions ~flame_open : Layout.t)]);
  [%expect
    {|
    ((ticks ((x 0) (y 0) (width 80) (height 1)))
     (controls ((x 0) (y 1) (width 80) (height 1)))
     (top_divider ((x 0) (y 2) (width 80) (height 1)))
     (stack ((x 0) (y 3) (width 29) (height 10)))
     (source ((x 0) (y 14) (width 29) (height 8)))
     (heap ((x 30) (y 3) (width 50) (height 17)))
     (flame ((x 30) (y 21) (width 50) (height 1)))
     (column_divider ((x 29) (y 3) (width 1) (height 19)))
     (row_divider ((x 0) (y 13) (width 29) (height 1)))
     (heap_divider ((x 30) (y 20) (width 50) (height 1)))
     (bottom_divider ((x 0) (y 22) (width 80) (height 1)))
     (session ((x 0) (y 23) (width 80) (height 1))))
    ((ticks ((x 0) (y 0) (width 80) (height 1)))
     (controls ((x 0) (y 1) (width 80) (height 1)))
     (top_divider ((x 0) (y 2) (width 80) (height 1)))
     (stack ((x 0) (y 3) (width 29) (height 10)))
     (source ((x 0) (y 14) (width 29) (height 8)))
     (heap ((x 30) (y 3) (width 50) (height 11)))
     (flame ((x 30) (y 15) (width 50) (height 7)))
     (column_divider ((x 29) (y 3) (width 1) (height 19)))
     (row_divider ((x 0) (y 13) (width 29) (height 1)))
     (heap_divider ((x 30) (y 14) (width 50) (height 1)))
     (bottom_divider ((x 0) (y 22) (width 80) (height 1)))
     (session ((x 0) (y 23) (width 80) (height 1))))
    |}]
;;
