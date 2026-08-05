open! Core
open Jsip_types
open Jsip_parsing
open Jsip_replay
open Jsip_web_components

let replay_of_fixture name =
  let parsed_info =
    Dump_reader.read [%string "../../../testing/expected/%{name}.dump"]
    |> Or_error.ok_exn
  in
  Replay.create (Call_stack.create ~parsed_info)
;;

let scene replay ~step =
  let { Replay.Step.structures; nodes; new_addresses; _ } =
    Replay.step_exn replay ~step
  in
  Heap_scene.build
    ~structures
    ~nodes
    ~new_addresses
    ~folds:(Set.empty (module Heap_scene.Fold_key))
    ~filter:""
    ~sort_by_address:false
    ~accordion:false
;;

let%expect_test "each tier lays every box out, roomier as detail grows" =
  let replay = replay_of_fixture "map_fold" in
  let roots, (_ : Heap_scene.Stats.t) =
    scene replay ~step:(Replay.length replay - 1)
  in
  let layouts = Heap_layout.all roots in
  Array.iteri layouts ~f:(fun tier (layout : Heap_layout.Tier_layout.t) ->
    print_endline
      [%string
        "tier %{tier#Int}: %{List.length layout.placed#Int} boxes · \
         %{Float.round_nearest layout.width#Float}×%{Float.round_nearest \
         layout.height#Float}"]);
  (* the same boxes at every tier, growing monotonically *)
  let counts =
    Array.map layouts ~f:(fun layout -> List.length layout.placed)
  in
  let same_boxes = Array.for_all counts ~f:(fun count -> count = counts.(0)) in
  let growing =
    Array.for_alli layouts ~f:(fun tier layout ->
      tier = 0 || Float.( >= ) layout.width layouts.(tier - 1).width)
  in
  print_endline
    [%string "same boxes %{same_boxes#Bool} · growing %{growing#Bool}"];
  [%expect {|
    tier 0: 8 boxes · 32.×282.
    tier 1: 8 boxes · 168.×902.
    tier 2: 8 boxes · 205.×1150.
    tier 3: 8 boxes · 220.×1800.
    same boxes true · growing true
    |}]
;;

let%expect_test "a parent's box is centered over the spread of its children" =
  let replay = replay_of_fixture "map_nested" in
  let roots, (_ : Heap_scene.Stats.t) = scene replay ~step:1 in
  let layout = Heap_layout.compute roots ~tier:1 in
  List.iter layout.placed ~f:(fun (placed : Heap_layout.Placed.t) ->
    let box = Map.find_exn layout.pos placed.id in
    print_endline
      [%string
        "%{placed.id} d%{placed.depth#Int} %{placed.edge_label} → \
         x=%{Float.round_nearest box.x#Float} \
         y=%{Float.round_nearest box.y#Float} \
         w=%{Float.round_nearest box.w#Float}"]);
  [%expect {|
    1: d0  → x=0. y=30. w=161.
    2: d0  → x=5. y=217. w=161.
    2:0 d1 l → x=0. y=294. w=30.
    2:1 d1 r → x=54. y=294. w=116.
    |}]
;;

let%expect_test "zoom stops and tiers round-trip through each other" =
  List.iter [ 0.3; 0.62; 1.; 1.35; 2.; 2.9; 4. ] ~f:(fun k ->
    let tier = Heap_layout.tier_for ~k in
    let back = Heap_layout.k_for_tier tier in
    print_endline
      [%string
        "k=%{sprintf \"%.2f\" k} → tier %{sprintf \"%.2f\" tier} → \
         k=%{sprintf \"%.2f\" back}"]);
  [%expect {|
    k=0.30 → tier 0.16 → k=0.30
    k=0.62 → tier 1.00 → k=0.62
    k=1.00 → tier 1.61 → k=1.00
    k=1.35 → tier 2.00 → k=1.35
    k=2.00 → tier 2.51 → k=2.00
    k=2.90 → tier 3.00 → k=2.90
    k=4.00 → tier 3.00 → k=2.90
    |}]
;;

let%expect_test "content switches tiers only once geometry has grown" =
  List.iter [ 0.; 0.5; 0.84; 0.85; 1.; 1.9; 3. ] ~f:(fun tier_f ->
    let split = Heap_layout.split tier_f in
    print_endline
      [%string
        "tier_f %{sprintf \"%.2f\" tier_f}: geometry %{split.a#Int}→\
         %{split.b#Int} at %{sprintf \"%.2f\" split.f} · content \
         %{Heap_layout.content_tier tier_f#Int}"]);
  [%expect {|
    tier_f 0.00: geometry 0→1 at 0.00 · content 0
    tier_f 0.50: geometry 0→1 at 0.59 · content 0
    tier_f 0.84: geometry 0→1 at 0.99 · content 0
    tier_f 0.85: geometry 0→1 at 1.00 · content 1
    tier_f 1.00: geometry 1→2 at 0.00 · content 1
    tier_f 1.90: geometry 1→2 at 1.00 · content 2
    tier_f 3.00: geometry 3→3 at 0.00 · content 3
    |}]
;;

let%expect_test "interpolated boxes sit between their two tier homes" =
  let replay = replay_of_fixture "map_nested" in
  let roots, (_ : Heap_scene.Stats.t) = scene replay ~step:1 in
  let layouts = Heap_layout.all roots in
  let id =
    Heap_layout.key_id
      (List.hd_exn roots).Heap_scene.Root.node.Heap_scene.Node.key
  in
  List.iter [ 1.0; 1.4; 1.85 ] ~f:(fun tier_f ->
    match Heap_layout.box_of layouts ~tier_f ~id with
    | None -> print_endline "none"
    | Some box ->
      print_endline
        [%string
          "tier_f %{sprintf \"%.2f\" tier_f}: w=%{Float.round_nearest \
           box.w#Float} h=%{Float.round_nearest box.h#Float}"]);
  [%expect {|
    tier_f 1.00: w=161. h=23.
    tier_f 1.40: w=166. h=31.
    tier_f 1.85: w=173. h=39.
    |}]
;;
