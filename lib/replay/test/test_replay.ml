open! Core
open Jsip_types
open Jsip_parsing
open Jsip_replay

(* every dump here is a golden fixture — verbatim compiler output vendored
   under testing/expected/ (see testing/README.md) *)
let replay_of_fixture name =
  let parsed_info = Queue.create () in
  Dump_reader.read_until_empty
    [%string "../../../testing/expected/%{name}.dump"]
    ~store_data:(Queue.enqueue parsed_info);
  Replay.create (Call_stack.create ~parsed_info)
;;

let show_steps replay =
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.frames; new_addresses; description; _ } =
        Replay.step_exn replay ~step
      in
      let stack = List.length frames in
      let fresh = Set.length new_addresses in
      print_endline
        [%string
          "%{step#Int}: [%{stack#Int} live, %{fresh#Int} new] %{description}"])
;;

let%expect_test "a nested dump stacks its frames" =
  (* map_nested: the inner [M.add] fires at depth 2 inside the outer
     [M.add]'s frame, so the outer call's own event only lands afterwards,
     back at depth 1 *)
  let replay = replay_of_fixture "map_nested" in
  show_steps replay;
  [%expect
    {|
    0: [1 live, 1 new] M.add "inner" 2 M.empty — map_nested.ml:5
    1: [1 live, 2 new] M.add "outer" 1 (M.add "inner" 2 M.empty) — map_nested.ml:5
    |}]
;;

let%expect_test "map_fold interleaves depths and reuses structure" =
  let replay = replay_of_fixture "map_fold" in
  show_steps replay;
  [%expect
    {|
    0: [1 live, 1 new] M.add "b" 2 M.empty — map_fold.ml:8
    1: [1 live, 2 new] M.add "a" 1 (M.add "b" 2 M.empty) — map_fold.ml:8
    2: [2 live, 1 new] M.add k (v * 2) acc — map_fold.ml:12
    3: [2 live, 2 new] M.add k (v * 2) acc — map_fold.ml:12
    4: [1 live, 0 new] M.fold (fun k v acc -> M.add k (v * 2) acc) m M.empty — map_fold.ml:10
    |}]
;;

let%expect_test "a mutable queue keeps its identity while growing" =
  let replay = replay_of_fixture "queue_basic" in
  show_steps replay;
  [%expect
    {|
    0: [1 live, 1 new] Queue.create () — queue_basic.ml:7
    1: [1 live, 1 new] Queue.add "x" q — queue_basic.ml:8
    2: [1 live, 1 new] Queue.add "y" q — queue_basic.ml:9
    3: [1 live, 1 new] Queue.push "z" q — queue_basic.ml:10
    4: [1 live, 0 new] Queue.pop q — queue_basic.ml:11
    |}]
;;

let%expect_test "the registry drops structures the GC collected" =
  let replay = replay_of_fixture "map_registry_gc" in
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.call; _ } = Replay.step_exn replay ~step in
      let registry =
        List.map call.info.registry ~f:(fun (id, address) ->
          [%string "%{id#Int}↦%{Snapshot.Address.display address}"])
        |> String.concat ~sep:" "
      in
      print_endline [%string "%{step#Int}: registry %{registry}"]);
  [%expect
    {|
    0: registry 1↦0x7647edff0770
    1: registry 2↦0x7647edffffd8
    |}]
;;

let%expect_test "files come back in first-appearance order" =
  let replay = replay_of_fixture "map_basic" in
  print_s [%sexp (Replay.files replay : string list)];
  [%expect {| (testing/cases/map_basic.ml) |}]
;;

let%expect_test "structures live until the registry drops them" =
  let replay = replay_of_fixture "queue_of_maps" in
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.structures; _ } = Replay.step_exn replay ~step in
      let show =
        List.map
          structures
          ~f:(fun { Replay.Structure.id; snapshot; is_current; _ } ->
            let mark = match is_current with true -> "▸" | false -> " " in
            [%string
              "%{mark}#%{id#Int} %{Snapshot.Ds_type.display \
               snapshot.ds_type}"])
        |> String.concat ~sep:"  "
      in
      print_endline [%string "step %{step#Int}: %{show}"]);
  [%expect
    {|
    step 0: ▸#1 map
    step 1:  #1 map  ▸#2 queue
    step 2:  #1 map  ▸#2 queue
    |}]
;;
