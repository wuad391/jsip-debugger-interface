open! Core
open Jsip_types
open Jsip_parsing
open Jsip_replay

(* a nested dump: [M.of_list] calls [M.add] (depth 2), then [M.remove] runs
   at depth 1. Event 2's root is event 3's root too, so step 2 allocates
   nothing new. *)
let nested_dump =
  {|{(event (id 1) (loc ((file_path dir/maps.ml) (line_number 4) (char_range (10 23)))) (fn (Function_name M.of_list)) (args ((No_label (expression (Unnamed "[\"a\", 1]"))))) (registry ((1 0x1a0))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x1a0) (block ((v (String a)) (d (Int 1)))) (children ()))))))
{(event (id 2) (loc ((file_path dir/maps.ml) (line_number 9) (char_range (2 15)))) (fn (Function_name M.add)) (args ((No_label (expression (Unnamed "\"b\""))) (Labelled (label data) (expression (Unnamed 2))))) (registry ((1 0x1a0) (2 0x2b0))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x2b0) (block ((v (String a)) (d (Int 1)))) (children (((virtual_address 0x2b8) (block ((v (String b)) (d (Int 2)))) (children ())))))))))
}}{(event (id 3) (loc ((file_path dir/maps.ml) (line_number 5) (char_range (10 24)))) (fn (Function_name M.remove)) (args ((No_label (expression (Unnamed "\"a\""))))) (registry ((1 0x1a0) (2 0x2b0) (3 0x2b8))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x2b8) (block ((v (String b)) (d (Int 2)))) (children ()))))))
}|}
;;

let replay_of_dump dump =
  let file = "replay_dump.txt" in
  Out_channel.write_all file ~data:dump;
  let parsed_info = Queue.create () in
  Dump_reader.read_until_empty file ~store_data:(Queue.enqueue parsed_info);
  Replay.create (Call_stack.create ~parsed_info)
;;

let%expect_test "per-step frames, descriptions, and fresh addresses" =
  let replay = replay_of_dump nested_dump in
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.call = _; frames; new_addresses; description } =
        Replay.step_exn replay ~step
      in
      let frames =
        List.map frames ~f:(fun frame ->
          Function_info.display frame.info.function_info)
        |> String.concat ~sep:" > "
      in
      let fresh =
        Set.to_list new_addresses
        |> List.map ~f:Snapshot.Address.display
        |> String.concat ~sep:" "
      in
      print_endline [%string "%{step#Int}: %{description}"];
      print_endline [%string "   stack: %{frames}"];
      print_endline [%string "   fresh: %{fresh}"]);
  [%expect
    {|
    0: M.of_list ["a", 1] — maps.ml:4
       stack: M.of_list
       fresh: 0x1a0
    1: M.add "b" ~data:2 — maps.ml:9
       stack: M.of_list > M.add
       fresh: 0x2b0 0x2b8
    2: M.remove "a" — maps.ml:5
       stack: M.remove
       fresh:
    |}]
;;

let%expect_test "files come back in first-appearance order" =
  let replay = replay_of_dump nested_dump in
  print_s [%sexp (Replay.files replay : string list)];
  [%expect {| (dir/maps.ml) |}]
;;

(* the checked-in demo the README points at — keep it parsing and replaying *)
let%expect_test "the bundled demo dump replays" =
  let parsed_info = Queue.create () in
  Dump_reader.read_until_empty
    "../../../demo/maps.dump"
    ~store_data:(Queue.enqueue parsed_info);
  let replay = Replay.create (Call_stack.create ~parsed_info) in
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.frames; new_addresses; description; call = _ } =
        Replay.step_exn replay ~step
      in
      let stack = List.length frames in
      let fresh = Set.length new_addresses in
      print_endline
        [%string
          "%{step#Int}: [%{stack#Int} live, %{fresh#Int} new] %{description}"]);
  [%expect
    {|
    0: [1 live, 3 new] M.of_list [ "beta", 2; "delta", 1; "kappa", 4 ] — maps.ml:5
    1: [1 live, 0 new] M.add "gamma" 3 t — maps.ml:6
    2: [2 live, 0 new] M.add "gamma" 3 OMITTED — maps.ml:6
    3: [3 live, 1 new] M.add "gamma" 3 OMITTED — maps.ml:6
    4: [2 live, 1 new] M.bal OMITTED — maps.ml:6
    5: [1 live, 1 new] M.bal OMITTED — maps.ml:6
    6: [1 live, 1 new] M.remove "beta" t' — maps.ml:7
    |}]
;;
