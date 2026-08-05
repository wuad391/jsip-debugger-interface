open! Core
open Jsip_types
open Jsip_parsing
open Jsip_replay

(* every dump here is a golden fixture — verbatim compiler output vendored
   under testing/expected/ (see testing/README.md) *)
let replay_of_fixture name =
  let parsed_info =
    Dump_reader.read [%string "../../../testing/expected/%{name}.dump"]
    |> Or_error.ok_exn
  in
  Replay.create (Call_stack.create ~parsed_info)
;;

let show_steps replay =
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.call; frames; new_addresses; _ } =
        Replay.step_exn replay ~step
      in
      let description = Replay.description call in
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
    0: [2 live, 1 new] M.add "inner" 2 M.empty — map_nested.ml:5
    1: [1 live, 2 new] M.add "outer" 1 (M.add "inner" 2 M.empty) — map_nested.ml:5
    |}]
;;

let%expect_test "map_fold interleaves depths and reuses structure" =
  let replay = replay_of_fixture "map_fold" in
  show_steps replay;
  [%expect
    {|
    0: [2 live, 1 new] M.add "b" 2 M.empty — map_fold.ml:8
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
        List.map call.info.registry ~f:(fun (entry : Registry_entry.t) ->
          [%string
            "%{Registry_entry.display entry}↦%{Snapshot.Address.display \
             entry.address}"])
        |> String.concat ~sep:" "
      in
      print_endline [%string "%{step#Int}: registry %{registry}"]);
  [%expect
    {|
    0: registry #1↦0x7f13285ee7e8
    1: registry #2↦0x7f13285fffd8
    |}]
;;

let show_visibility replay =
  List.iter
    (List.init (Replay.length replay) ~f:Fn.id)
    ~f:(fun step ->
      let { Replay.Step.structures; _ } = Replay.step_exn replay ~step in
      let show =
        List.map structures ~f:(fun (structure : Replay.Structure.t) ->
          let visibility =
            Sexp.to_string
              [%sexp (structure.visibility : Replay.Visibility.t)]
          in
          [%string
            "#%{structure.id#Int} %{Replay.Structure.display structure} \
             %{visibility}"])
        |> String.concat ~sep:"  "
      in
      print_endline [%string "step %{step#Int}: %{show}"])
;;

let%expect_test "a rebound name shadows the versions it displaces" =
  (* map_basic is four [let m = ...]s over the same name: every version stays
     alive and every registry entry says [m], so the name alone cannot say
     which one the program can still reach. The binder can. *)
  let replay = replay_of_fixture "map_basic" in
  show_visibility replay;
  [%expect
    {|
    step 0: #1 m In_scope
    step 1: #1 m Shadowed  #2 m In_scope
    step 2: #1 m Shadowed  #2 m Shadowed  #3 m In_scope
    |}]
;;

let%expect_test "one binding per name shadows nothing" =
  (* [map_versions] builds its chain under [m1] and [m2] rather than one
     rebound name, and both stay reachable for as long as [grow] runs. The
     versions with no name of their own — the innermost [add], and the two
     the toplevel builds after [grow] has returned — are the [Unknown] ones:
     never observed under a name, so no name can be taken away from them. *)
  let replay = replay_of_fixture "map_versions" in
  show_visibility replay;
  [%expect
    {|
    step 0: #1 #1 Unknown
    step 1: #1 #1 Unknown  #2 m1 In_scope
    step 2: #1 #1 Unknown  #2 m1 In_scope  #4 m2 In_scope
    step 3: #1 #1 Unknown  #2 m1 In_scope  #4 m2 In_scope  #7 #7 Unknown
    step 4: #7 #7 Unknown  #11 #11 Unknown
    |}]
;;

let%expect_test "a structure never observed under a name has no verdict" =
  (* the fold's accumulators are results of an inner call bound to nothing,
     so no name reaches them and none can be taken away: [Unknown], drawn at
     full strength. That is the honest answer rather than a cautious one —
     the closure's own [acc] does reach them, and a parameter is not a
     binding this pass observes. *)
  let replay = replay_of_fixture "map_fold" in
  show_visibility replay;
  [%expect
    {|
    step 0: #1 #1 Unknown
    step 1: #1 #1 Unknown  #2 m In_scope
    step 2: #1 #1 Unknown  #2 m In_scope  #4 #4 Unknown
    step 3: #1 #1 Unknown  #2 m In_scope  #4 #4 Unknown  #5 #5 Unknown
    step 4: #1 #1 Unknown  #2 m In_scope  #4 #4 Unknown  #5 doubled In_scope
    |}]
;;

let%expect_test "a name outlives its scope, and a structure outlives both" =
  (* multi_file is three modules. [Basket.deliveries] builds a queue under
     [q] and returns it; [Main] binds what comes back as [arrivals]. So #4 is
     one structure wearing three verdicts — reachable as [q] while
     [deliveries] runs (steps 2-4), alive but nameless once it has returned
     (5-6), reachable again as [arrivals] once Main has bound it (7-8).

     Steps 5-6 are the case a name alone cannot see: nothing shadows [q], the
     structure is alive and on screen, and no name reaches it. They are also
     why a binder carries its unit — [q] and [stock] are bound in different
     files here, and stamps start again in each. *)
  let replay = replay_of_fixture "multi_file" in
  show_visibility replay;
  [%expect
    {|
    step 0: #1 #1 Unknown
    step 1: #1 #1 Unknown  #2 #2 Unknown
    step 2: #1 #1 Unknown  #2 #2 Unknown  #4 q In_scope
    step 3: #1 #1 Unknown  #2 #2 Unknown  #4 q In_scope
    step 4: #1 #1 Unknown  #2 #2 Unknown  #4 q In_scope
    step 5: #1 #1 Unknown  #2 #2 Unknown  #4 q Out_of_scope  #8 #8 Unknown
    step 6: #1 #1 Unknown  #2 #2 Unknown  #4 q Out_of_scope  #8 #8 Unknown  #11 #11 Unknown
    step 7: #1 #1 Unknown  #2 #2 Unknown  #4 arrivals In_scope  #8 #8 Unknown  #11 #11 Unknown
    step 8: #1 #1 Unknown  #2 #2 Unknown  #4 arrivals In_scope  #8 #8 Unknown  #11 stock In_scope
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
