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

let rows_at
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
  Heap_outline.rows
    ~structures
    ~nodes
    ~new_addresses
    ~folds
    ~filter
    ~sort_by_address
    ~accordion
;;

(* the outline as the pane draws it: a blank line between top-level
   structures, exactly the TUI heap pane's picture minus its colors *)
let show rows =
  List.iteri rows ~f:(fun index (row : Heap_outline.Row.t) ->
    (match index > 0 && row.depth = 0 with
     | true -> print_endline ""
     | false -> ());
    print_endline (Heap_outline.Row.text row))
;;

(* what the colors say, for the tests that are about them *)
let show_marks rows =
  List.iter rows ~f:(fun (row : Heap_outline.Row.t) ->
    let marks =
      List.filter_opt
        [ (match row.faded with true -> Some "faded" | false -> None)
        ; (match row.matched with true -> None | false -> Some "dimmed")
        ; (match row.is_current with true -> Some "current" | false -> None)
        ; (match row.is_new with true -> Some "new" | false -> None)
        ; (match row.is_pointer with true -> Some "pointer" | false -> None)
        ]
    in
    let marks =
      match marks with
      | [] -> ""
      | marks -> [%string "   [%{String.concat marks ~sep:\" \"}]"]
    in
    print_endline [%string "%{Heap_outline.Row.text row}%{marks}"])
;;

let%expect_test "a map reads as its bindings, one row each" =
  (* the TUI pane's own picture of this step, guides and all: two versions of
     [m], the shadowed one first *)
  let replay = replay_of_fixture "map_basic" in
  show (rows_at replay ~step:1);
  [%expect
    {|
    ▾ m  int M.t  1 binding  1 node · 56 B · shadowed
    └─   "a" → 1

    ▾ m  int M.t  2 bindings  2 nodes · 112 B  new
    ├─   "a" → 1  new
    └─   "b" → 2  new
    |}]
;;

let%expect_test "a queue is a record over its cells" =
  (* [{length; first; last}] is a record OVER the contents, so its own
     summary is the structure's row and the cells are the rows under it —
     where a map's root IS a binding and the row counts them instead *)
  let replay = replay_of_fixture "queue_basic" in
  show (rows_at replay ~step:2);
  [%expect
    {|
    ▾ q  string Queue.t  length 2  3 nodes · 104 B
    ├─   "x"
    └─   "y"  new
    |}]
;;

let%expect_test "boxed map data hangs under half a binding" =
  (* the key stays on the binding's row and says [→]; its data is a block of
     its own, so it is the row below *)
  let replay = replay_of_fixture "map_data_kinds" in
  show (rows_at replay ~step:2);
  [%expect
    {|
    ▾ m  float M.t  1 binding  1 node · 72 B
    └─   "pi" → 3.14

    ▾ #2  float M.t  2 bindings  2 nodes · 144 B
    ├─   "pi" → 3.14
    └─   "e" → 2.71

    ▾ #4  (int * string) M.t  1 element  2 nodes · 96 B  new
    └─   "pair" →  new
       └─   d  1, "one"  new
    |}]
;;

let%expect_test "a hashtable's bucket array is plumbing and gets no row" =
  (* record → bucket array → chain: the record is a size over its contents
     and the array has nothing of its own to say, so neither takes a row and
     the entry sits directly under the structure *)
  let replay = replay_of_fixture "hashtbl_basic" in
  show (rows_at replay ~step:(Replay.length replay - 1));
  [%expect
    {|
    ▾ tbl  (string, int) Hashtbl.t  size 1  3 nodes · 208 B
    └─   "a" → 10
    |}]
;;

let%expect_test "a Core map is a record over a tagged tree" =
  (* Core reaches its maps through Base, whose map is a record holding the
     comparator and the tree — not the tree itself, the way the stdlib's is.
     The record is absorbed into the structure's row and Base's own
     [Leaf]/[Node] shapes below it read as the bindings they hold. *)
  let replay = replay_of_fixture "core_map_basic" in
  show (rows_at replay ~step:2);
  [%expect
    {|
    ▾ m  (string, int) Map.t  1 binding  2 nodes · 56 B · shadowed
    └─   "b" → 2

    ▾ m  (string, int) Map.t  2 bindings  3 nodes · 112 B · shadowed
    ├─   "b" → 2
    └─   "a" → 1

    ▾ m  (string, int) Map.t  3 bindings  3 nodes · 112 B  new
    ├─   "b" → 2  new
    ├─   ↗ "a" → 1
    └─   "c" → 3  new
    |}]
;;

let%expect_test "a Core hash queue chains through its elements" =
  (* the order-book shape: a doubly-linked list of key/data pairs, where an
     element's [next] stays on its own level while [v] steps down to the pair
     — and the ring closes on a block already listed, so the last row is a
     pointer *)
  let replay = replay_of_fixture "core_hash_queue" in
  show_marks (rows_at replay ~step:(Replay.length replay - 1));
  [%expect
    {|
    ▾ q  (string, int) Hash_queue.t  4 bindings  9 nodes · 240 B   [current]
    ├─   v  "a" → 1
    ├─   v  "b" → 2
    ├─   v  "c" → 3  new   [new]
    └─   ↗ null   [pointer]
    |}]
;;

let%expect_test "a shared subtree is listed once and pointed at after" =
  (* map_spine_sharing: two versions of a map share their spines, so the
     second version names what it shares rather than repeating it. The counts
     are of the latest WALK — a re-observed version re-walks only what
     changed, and the rows above what it shares are resolved through the node
     table, which is why five bindings sit over three nodes. *)
  let replay = replay_of_fixture "map_spine_sharing" in
  show_marks (rows_at replay ~step:(Replay.length replay - 1));
  [%expect
    {|
    ▾ m  int M.t  5 bindings  3 nodes · 168 B
    ├─   "f" → 6
    ├─   "d" → 4
    ├─   "b" → 2
    ├─   "h" → 8
    └─   "j" → 10
    ▾ bigger  int M.t  5 bindings  3 nodes · 168 B  new   [current new]
    ├─   "f" → 6  new   [new]
    ├─   ↗ "d" → 4   [pointer]
    ├─   "h" → 8  new   [new]
    ├─   "g" → 7  new   [new]
    └─   ↗ "j" → 10   [pointer]
    |}]
;;

let%expect_test "a referenced structure is listed inside its referrer" =
  (* the map is reachable only through the queue's cell, so it hangs there —
     with its own name, its own type and its own verdict *)
  let replay = replay_of_fixture "queue_of_maps" in
  show_marks (rows_at replay ~step:2);
  [%expect
    {|
    ▾ q  int M.t Queue.t  length 1  2 nodes · 48 B   [current]
    └─ ▾ m  int M.t  1 binding  1 node · 56 B
       └─   "k" → 1
    |}]
;;

let%expect_test "a user record's fields hang under it" =
  (* [User] has no skeleton of its own: the walker labels its fields from the
     schema the instrumentation derived, and the outline prints exactly that *)
  let replay = replay_of_fixture "user_types" in
  show (rows_at replay ~step:(Replay.length replay - 1));
  [%expect
    {|
      p  point  x=3  y=4  1 node · 24 B

    ▾ ts  trades  0  1 node · 24 B  new
    └─ ▾ hd  t  trade  101  3 nodes · 112 B
       ├─   tags  "buy", "limit"
       └─   span  1, 9
    |}]
;;

let%expect_test "folding a structure hides its rows behind a count" =
  let replay = replay_of_fixture "map_basic" in
  let folds =
    Set.singleton (module Heap_scene.Fold_key) (Heap_scene.Fold_key.root 2)
  in
  show (rows_at replay ~folds ~step:1);
  [%expect
    {|
    ▾ m  int M.t  1 binding  1 node · 56 B · shadowed
    └─   "a" → 1

    ▸ m  int M.t  2 bindings  2 nodes · 112 B  ⋯ 2  new
    |}]
;;

let%expect_test "the accordion leaves the walked structure open" =
  let replay = replay_of_fixture "map_basic" in
  show (rows_at replay ~accordion:true ~step:1);
  [%expect
    {|
    ▸ m  int M.t  1 binding  1 node · 56 B · shadowed  ⋯ 1

    ▾ m  int M.t  2 bindings  2 nodes · 112 B  new
    ├─   "a" → 1  new
    └─   "b" → 2  new
    |}]
;;

let%expect_test "the filter dims what it does not find, and lights whole \
                 structures"
  =
  (* [/queue] is a structure-level hit: everything under the queue's header
     stays lit, including the map listed inside it, and the rows of any
     structure it does not name go dim *)
  let replay = replay_of_fixture "queue_of_maps" in
  show_marks (rows_at replay ~filter:"queue" ~step:1);
  print_endline "";
  show_marks (rows_at replay ~filter:"\"k\"" ~step:1);
  [%expect
    {|
    ▾ m  int M.t  1 binding  1 node · 56 B   [dimmed]
    └─   "k" → 1   [dimmed]
      q  int M.t Queue.t  length 0  1 node · 24 B  new   [current new]

    ▾ m  int M.t  1 binding  1 node · 56 B   [dimmed]
    └─   "k" → 1
      q  int M.t Queue.t  length 0  1 node · 24 B  new   [dimmed current new]
    |}]
;;

let%expect_test "address order re-sorts the structures, nothing else" =
  let replay = replay_of_fixture "map_basic" in
  let addresses rows =
    List.filter_map rows ~f:(fun (row : Heap_outline.Row.t) ->
      match row.depth, row.address with
      | 0, Some address -> Some (Snapshot.Address.display address)
      | (_ : int), (None | Some _) -> None)
  in
  let registry_order = addresses (rows_at replay ~step:1) in
  let by_address =
    addresses (rows_at replay ~sort_by_address:true ~step:1)
  in
  print_s
    [%message
      (registry_order : string list)
        (by_address : string list)
        ~sorted:
          (List.equal
             String.equal
             (List.sort by_address ~compare:String.compare)
             by_address
           : bool)];
  [%expect
    {|
    ((registry_order (0x72d2a9feeb50 0x72d2a9fea718))
     (by_address (0x72d2a9fea718 0x72d2a9feeb50)) (sorted true))
    |}]
;;

let%expect_test "every step of the awkward dumps yields rows, and stops" =
  (* cycles, structures inside structures, shared payloads and a walk that
     observes two roots in one event: the shapes where a reader that trusts
     the wire's ids can loop or lose a node. Rows per step, so a change in
     what any of them reads shows up as a number moving. *)
  List.iter
    [ "queue_cycle"
    ; "queue_of_queues"
    ; "queue_transfer"
    ; "map_shared_payload"
    ; "map_record_nested"
    ; "core_doubly_linked"
    ; "core_hashtbl_basic"
    ; "multi_file"
    ]
    ~f:(fun name ->
      let replay = replay_of_fixture name in
      let counts =
        List.init (Replay.length replay) ~f:(fun step ->
          List.length (rows_at replay ~step))
      in
      print_s [%message name (counts : int list)]);
  [%expect
    {|
    (queue_cycle (counts (1 2 2)))
    (queue_of_queues (counts (1 2 2 2 2 2)))
    (queue_transfer (counts (1 2 3 4 2 4)))
    (map_shared_payload (counts (1 3 8 15)))
    (map_record_nested (counts (5)))
    (core_doubly_linked (counts (1 3 4 5)))
    (core_hashtbl_basic (counts (2 2 3 4)))
    (multi_file (counts (2 5 6 7 8 12 16 16 16)))
    |}]
;;

let%expect_test "the folds a row names are the canvas's own" =
  (* one key per box, shared by both readings: folding a row here folds the
     same box in the diagram, which is what lets the tabs agree *)
  let replay = replay_of_fixture "queue_of_maps" in
  List.iter (rows_at replay ~step:2) ~f:(fun (row : Heap_outline.Row.t) ->
    let fold = Sexp.to_string [%sexp (row.fold : Heap_scene.Fold_key.t)] in
    print_endline
      [%string
        "%{Heap_outline.Row.text row}   %{fold} foldable \
         %{row.foldable#Bool}"]);
  [%expect
    {|
    ▾ q  int M.t Queue.t  length 1  2 nodes · 48 B   ((structure_id 2)(path())) foldable true
    └─ ▾ m  int M.t  1 binding  1 node · 56 B   ((structure_id 1)(path())) foldable true
       └─   "k" → 1   ((structure_id 1)(path())) foldable false
    |}]
;;
