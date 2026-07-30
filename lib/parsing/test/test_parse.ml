open! Core
open Jsip_types
open Jsip_parsing

(* the golden dumps under testing/expected/ are verbatim output of real
   [-visual-replay] runs (see testing/README.md) — the tests here read them
   exactly as the interface will *)
let fixture name = [%string "../../../testing/expected/%{name}.dump"]

let all_fixtures =
  [ "map_alias_open"
  ; "map_basic"
  ; "map_data_kinds"
  ; "map_fold"
  ; "map_nested"
  ; "map_partial_neg"
  ; "map_registry_gc"
  ; "neg_plain"
  ; "neg_untracked"
  ; "queue_basic"
  ; "queue_mixed"
  ; "queue_of_maps"
  ; "set_basic"
  ; "set_ops"
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
      (registry ((1 0x763be65f19e8)))
      (snapshot
       ((ds_type Map)
        (root_node
         ((virtual_address 0x763be65f19e8)
          (block ((l (Int 0)) (v (String a)) (d (Int 1)) (r (Int 0))))
          (children ())))))))
    |}]
;;

let%expect_test "read a whole real dump into Call.Info values" =
  Dump_reader.read_until_empty (fixture "map_basic") ~store_data:(fun info ->
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
  Dump_reader.read_until_empty
    (fixture "map_nested")
    ~store_data:(fun info ->
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
    let events = ref 0 in
    Dump_reader.read_until_empty
      (fixture name)
      ~store_data:(fun (_ : Call.Info.t) -> incr events);
    print_endline [%string "%{name}: %{!events#Int} events"]);
  [%expect
    {|
    map_alias_open: 1 events
    map_basic: 3 events
    map_data_kinds: 3 events
    map_fold: 5 events
    map_nested: 2 events
    map_partial_neg: 0 events
    map_registry_gc: 2 events
    neg_plain: 0 events
    neg_untracked: 0 events
    queue_basic: 5 events
    queue_mixed: 7 events
    queue_of_maps: 3 events
    set_basic: 3 events
    set_ops: 5 events
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

let%expect_test "a dump that does not return to depth 0 is rejected" =
  let file = "truncated_dump.txt" in
  let truncated =
    In_channel.read_lines (fixture "map_basic")
    |> Fn.flip List.take 2
    |> String.concat ~sep:"\n"
  in
  Out_channel.write_all file ~data:(truncated ^ "\n}}\n");
  Expect_test_helpers_core.show_raise (fun () ->
    Dump_reader.read_until_empty
      file
      ~store_data:(ignore : Call.Info.t -> unit));
  [%expect {| (raised (Failure "DUMP READER: Incorrect file ending!")) |}]
;;
