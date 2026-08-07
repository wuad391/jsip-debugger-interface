open! Core
open Jsip_types
open Jsip_parsing

(* the golden dumps under testing/expected/ are verbatim output of real
   [-visual-replay] runs (see testing/README.md) — the tests here read them
   exactly as the interface will *)
let fixture name = [%string "../../../testing/expected/%{name}.dump"]

let all_fixtures =
  [ "core_deque_basic"
  ; "core_doubly_linked"
  ; "core_fdeque_basic"
  ; "core_hash_queue"
  ; "core_hash_set_basic"
  ; "core_hashtbl_basic"
  ; "core_linked_queue"
  ; "core_map_basic"
  ; "core_map_legacy"
  ; "core_queue_wrap"
  ; "core_set_basic"
  ; "core_stack_basic"
  ; "delta_map_basic"
  ; "delta_map_registry_gc"
  ; "delta_map_versions"
  ; "dynarray_basic"
  ; "hashtbl_basic"
  ; "map_alias_open"
  ; "map_array_payload"
  ; "map_basic"
  ; "map_data_kinds"
  ; "map_fold"
  ; "map_list_payload"
  ; "map_nested"
  ; "map_partial_neg"
  ; "map_record_nested"
  ; "map_registry_gc"
  ; "map_rewalk"
  ; "map_shared_payload"
  ; "map_spine_sharing"
  ; "map_versions"
  ; "map_wide_payload"
  ; "multi_file"
  ; "neg_plain"
  ; "neg_untracked"
  ; "queue_basic"
  ; "queue_cycle"
  ; "queue_mixed"
  ; "queue_of_closures"
  ; "queue_of_maps"
  ; "queue_of_queues"
  ; "queue_transfer"
  ; "queue_wide_tuple"
  ; "set_basic"
  ; "set_ops"
  ; "stack_basic"
  ; "stdout_mixed"
  ; "user_floats"
  ; "user_types"
  ]
;;

let%expect_test "unsexp one real event line" =
  let line = In_channel.read_lines (fixture "map_basic") |> List.hd_exn in
  let line = String.chop_prefix_exn line ~prefix:"{" in
  print_s [%sexp (Dump_wire.of_string line : Dump_wire.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((id 1)
      (loc
       ((file_path testing/cases/map_basic.ml) (line_number 7)
        (char_range (10 23))))
      (fn (Function_name M.add))
      (args
       ((No_label (expression (Unnamed "\"a\"")))
        (No_label (expression (Unnamed 1))) (No_label (expression (Unnamed m)))))
      (registry ((1 0x72d2a9feeb50 m)))
      (ty ((printed "int M.t") (params ((key string) (data int)))))
      (binder Map_basic.m_478) (scope ((m Map_basic.m_478)))
      (snapshot
       ((ds_type Map)
        (root_node
         ((id 1) (virtual_address 0x72d2a9feeb50)
          (block ((l (Int 0)) (v (String a)) (d (Int 1)) (r (Int 0))))
          (children ())))))))
    |}]
;;

(* dumps from a compiler predating the [ty] field parse all the same — the
   field is optional, not a format version *)
let%expect_test "an event without a ty field reads as None" =
  (* [ty] arrived after the first dumps did, so the reader treats it as
     optional; everything else is the current wire *)
  let line =
    {|(event (id 1) |}
    ^ {|(loc ((file_path t.ml) (line_number 1) (char_range (0 1)))) |}
    ^ {|(fn (Function_name M.add)) (args ()) (registry ((1 0x10))) |}
    ^ {|(snapshot ((ds_type Map) (root_node ((id 1) (virtual_address 0x10) |}
    ^ {|(block ()) (children ()))))))|}
  in
  let wire = Dump_wire.of_string line |> Or_error.ok_exn in
  print_s [%sexp (wire.ty : Type_info.t option)];
  [%expect {| () |}]
;;

(* An event that states no scope and an event whose scope is empty are
   different claims — the first is an older compiler, the second a program
   point with nothing tracked in scope — and only an option can hold both. *)
let%expect_test "a stated empty scope is not a missing one" =
  let event ~fields =
    {|(event (id 1) |}
    ^ {|(loc ((file_path t.ml) (line_number 1) (char_range (0 1)))) |}
    ^ {|(fn (Function_name M.add)) (args ()) (registry ((1 0x10))) |}
    ^ fields
    ^ {|(snapshot ((ds_type Map) (root_node ((id 1) (virtual_address 0x10) |}
    ^ {|(block ()) (children ()))))))|}
  in
  let show line =
    let wire = Dump_wire.of_string line |> Or_error.ok_exn in
    print_s
      [%message
        ""
          ~binder:(wire.binder : Scope.Binder.t option)
          ~scope:(wire.scope : Scope.t option)]
  in
  show (event ~fields:"");
  [%expect {| ((binder ()) (scope ())) |}];
  show (event ~fields:{|(scope ()) |});
  [%expect {| ((binder ()) (scope (()))) |}];
  show (event ~fields:{|(binder T.m_9) (scope ((m T.m_9))) |});
  [%expect {| ((binder (T.m_9)) (scope (((m T.m_9))))) |}]
;;

let%expect_test "read a whole real dump into Call.Info values" =
  Dump_reader.read (fixture "map_basic")
  |> Or_error.ok_exn
  |> Queue.iter ~f:(fun info ->
    let { Call.Info.depth; id; function_info; location; arguments; _ } =
      info
    in
    let args =
      List.map arguments ~f:Argument.display |> String.concat ~sep:" "
    in
    print_endline
      [%string
        "depth %{depth#Int} id %{id#Int}: %{Function_info.display \
         function_info} %{args} @ %{Location.display location}"]);
  [%expect
    {|
    depth 1 id 1: M.add "a" 1 m @ map_basic.ml:7
    depth 1 id 2: M.add "b" 2 m @ map_basic.ml:8
    depth 1 id 3: M.remove "a" m @ map_basic.ml:9
    |}]
;;

let%expect_test "nested events carry their marker-derived depths" =
  Dump_reader.read (fixture "map_nested")
  |> Or_error.ok_exn
  |> Queue.iter ~f:(fun info ->
    print_endline
      [%string
        "depth %{info.Call.Info.depth#Int}: %{Function_info.display \
         info.function_info}"]);
  [%expect {|
    depth 2: M.add
    depth 1: M.add
    |}]
;;

let%expect_test "every golden dump parses end to end" =
  List.iter all_fixtures ~f:(fun name ->
    let events =
      Queue.length (Dump_reader.read (fixture name) |> Or_error.ok_exn)
    in
    print_endline [%string "%{name}: %{events#Int} events"]);
  [%expect
    {|
    core_deque_basic: 5 events
    core_doubly_linked: 4 events
    core_fdeque_basic: 3 events
    core_hash_queue: 4 events
    core_hash_set_basic: 4 events
    core_hashtbl_basic: 4 events
    core_linked_queue: 3 events
    core_map_basic: 4 events
    core_map_legacy: 3 events
    core_queue_wrap: 8 events
    core_set_basic: 3 events
    core_stack_basic: 5 events
    delta_map_basic: 3 events
    delta_map_registry_gc: 2 events
    delta_map_versions: 5 events
    dynarray_basic: 5 events
    hashtbl_basic: 6 events
    map_alias_open: 1 events
    map_array_payload: 1 events
    map_basic: 3 events
    map_data_kinds: 3 events
    map_fold: 5 events
    map_list_payload: 1 events
    map_nested: 2 events
    map_partial_neg: 0 events
    map_record_nested: 1 events
    map_registry_gc: 2 events
    map_rewalk: 3 events
    map_shared_payload: 4 events
    map_spine_sharing: 6 events
    map_versions: 5 events
    map_wide_payload: 1 events
    multi_file: 9 events
    neg_plain: 0 events
    neg_untracked: 0 events
    queue_basic: 5 events
    queue_cycle: 3 events
    queue_mixed: 8 events
    queue_of_closures: 3 events
    queue_of_maps: 3 events
    queue_of_queues: 6 events
    queue_transfer: 6 events
    queue_wide_tuple: 2 events
    set_basic: 3 events
    set_ops: 5 events
    stack_basic: 4 events
    stdout_mixed: 3 events
    user_floats: 2 events
    user_types: 3 events
    |}]
;;

(* No stdlib Map/Set/Queue function takes a labelled argument, so the golden
   dumps only ever show [No_label]; the wire carries labels all the same
   (vreplay/sexp.mli), and [OMITTED] displays as [_]. *)
let%expect_test "labelled and optional arguments carry their labels" =
  let args =
    {|((No_label (expression (Unnamed m)))
      (Labelled (label key) (expression (Unnamed "\"a\"")))
      (Optional (label eq) (expression (Unnamed OMITTED))))|}
  in
  let args = [%of_sexp: Argument.t list] (Sexp.of_string args) in
  print_s [%sexp (args : Argument.t list)];
  print_endline (List.map args ~f:Argument.display |> String.concat ~sep:" ");
  [%expect
    {|
    ((No_label (expression (Unnamed m)))
     (Labelled (label key) (expression (Unnamed "\"a\"")))
     (Optional (label eq) (expression (Unnamed OMITTED))))
    m ~key:"a" ?eq:_
    |}]
;;

let%expect_test "parse over a dump's contents agrees with read, exactly" =
  (* the web interface fetches the dump over HTTP and feeds [parse]; the TUI
     [read]s the file — they must be one reader *)
  let disagreements =
    List.count all_fixtures ~f:(fun name ->
      let from_file = Dump_reader.read (fixture name) in
      let from_memory =
        Dump_reader.parse (In_channel.read_all (fixture name))
      in
      not
        (Sexp.equal
           [%sexp (from_file : Call.Info.t Queue.t Or_error.t)]
           [%sexp (from_memory : Call.Info.t Queue.t Or_error.t)]))
  in
  print_s [%message (disagreements : int)];
  [%expect {| (disagreements 0) |}]
;;

let%expect_test "a dump that does not return to depth 0 is rejected" =
  let file = "truncated_dump.txt" in
  let truncated =
    In_channel.read_lines (fixture "map_basic")
    |> Fn.flip List.take 2
    |> String.concat ~sep:"\n"
  in
  Out_channel.write_all file ~data:(truncated ^ "\n}}\n");
  print_s [%sexp (Dump_reader.read file : Call.Info.t Queue.t Or_error.t)];
  [%expect
    {|
    (Error
     ((line_number 3) ("dump does not return to depth 0" (depth -1) (line }}))))
    |}]
;;

(* ---- the delta wire: (registry_delta ((upserts ...) (drops ...))) ---- *)

(* a minimal event line whose registry portion is [fields]; everything else
   is constant so the tests read as registry stories *)
let delta_event ~id ~fields =
  {|(event (id |}
  ^ Int.to_string id
  ^ {|) (loc ((file_path t.ml) (line_number 1) (char_range (0 1)))) |}
  ^ {|(fn (Function_name M.add)) (args ()) |}
  ^ fields
  ^ {| (snapshot ((ds_type Map) (root_node ((id |}
  ^ Int.to_string id
  ^ {|) (virtual_address 0x10) (block ()) (children ()))))))|}
;;

let show_registries dump =
  match Dump_reader.parse dump with
  | Error error -> print_s [%sexp (error : Error.t)]
  | Ok infos ->
    Queue.iter infos ~f:(fun (info : Call.Info.t) ->
      print_s [%sexp (info.registry : Registry_entry.t list)])
;;

let%expect_test "deltas fold to each event's whole registry" =
  (* upsert = appear, re-address, or rename, keeping first-upsert order; drop
     = disappear. The folded lists are what the full echoes said. *)
  let dump =
    String.concat
      ~sep:"\n"
      [ delta_event
          ~id:1
          ~fields:{|(registry_delta ((upserts ((1 0x10 m))) (drops ())))|}
      ; delta_event
          ~id:2
          ~fields:{|(registry_delta ((upserts ((2 0x20))) (drops ())))|}
        (* id 1 moves and renames in place; id 2 drops *)
      ; delta_event
          ~id:3
          ~fields:
            {|(registry_delta ((upserts ((1 0x30 q) (3 0x40 r))) (drops (2))))|}
      ]
  in
  show_registries dump;
  [%expect
    {|
    ((1 0x10 m))
    ((1 0x10 m) (2 0x20))
    ((1 0x30 q) (3 0x40 r))
    |}]
;;

let%expect_test "a full registry still reads, and resets the fold" =
  (* dumps up to compiler PR #23 state the whole registry per event; a
     transitional compiler emits both forms at once and the full echo wins,
     with later deltas folding on from it *)
  let dump =
    String.concat
      ~sep:"\n"
      [ delta_event ~id:1 ~fields:{|(registry ((1 0x10 m)))|}
      ; delta_event
          ~id:2
          ~fields:
            ({|(registry ((1 0x10 m) (2 0x20))) |}
             ^ {|(registry_delta ((upserts ((9 0x90))) (drops ())))|})
      ; delta_event
          ~id:3
          ~fields:{|(registry_delta ((upserts ((3 0x30))) (drops (1))))|}
      ]
  in
  show_registries dump;
  [%expect
    {|
    ((1 0x10 m))
    ((1 0x10 m) (2 0x20))
    ((2 0x20) (3 0x30))
    |}]
;;

let%expect_test "an event with neither registry form is rejected" =
  show_registries
    (delta_event ~id:1 ~fields:{|(ty ((printed t) (params ())))|});
  [%expect
    {|
    ((line_number 1)
     ("event carries neither registry nor registry_delta" (event_id 1)))
    |}]
;;

let%expect_test "the delta fixtures fold like their full-echo twins" =
  (* real output of the delta-emitting compiler branch (see
     testing/README.md): registry_gc drops the collected map (drops (1)),
     versions drops three of its five (drops (1 2 4)) *)
  List.iter [ "delta_map_registry_gc"; "delta_map_versions" ] ~f:(fun name ->
    let infos = Dump_reader.read (fixture name) |> Or_error.ok_exn in
    let last = Queue.last_exn infos in
    print_s
      [%message
        name
          ~events:(Queue.length infos : int)
          ~final_registry:(last.registry : Registry_entry.t list)]);
  [%expect
    {|
    (delta_map_registry_gc (events 2) (final_registry ((2 0x7d690adfffd8))))
    (delta_map_versions (events 5)
     (final_registry ((7 0x71786561aa68) (11 0x7178555fff48))))
    |}]
;;
