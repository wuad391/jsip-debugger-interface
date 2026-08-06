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
  let layouts = Heap_layout.all roots ~columns:1 in
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
  let same_boxes =
    Array.for_all counts ~f:(fun count -> count = counts.(0))
  in
  let growing =
    Array.for_alli layouts ~f:(fun tier layout ->
      tier = 0 || Float.( >= ) layout.width layouts.(tier - 1).width)
  in
  print_endline
    [%string "same boxes %{same_boxes#Bool} · growing %{growing#Bool}"];
  [%expect
    {|
    tier 0: 8 boxes · 32.×256.
    tier 1: 8 boxes · 252.×690.
    tier 2: 8 boxes · 279.×970.
    tier 3: 8 boxes · 319.×1882.
    same boxes true · growing true
    |}]
;;

(* The invariant the crossfade rests on. Zoom blends the two neighbouring
   tiers' layouts, so if each tier packed the structures for itself a
   structure could sit on a different row at each detail level and sail
   across its neighbours halfway through — which is what "the structures
   overlap when I scroll" looked like. Shelving is fixed once for all four
   tiers, and this walks the whole zoom range asking whether any two boxes
   ever intersect. *)
let%expect_test "no two boxes overlap at any zoom, at any column count" =
  let overlaps ~fixture ~columns =
    let replay = replay_of_fixture fixture in
    let roots, (_ : Heap_scene.Stats.t) =
      scene replay ~step:(Replay.length replay - 1)
    in
    let layouts = Heap_layout.all roots ~columns in
    let ids =
      List.map layouts.(0).placed ~f:(fun (placed : Heap_layout.Placed.t) ->
        placed.id)
    in
    List.sum
      (module Int)
      [ 0.; 0.4; 0.85; 1.; 1.5; 1.9; 2.; 2.6; 3. ]
      ~f:(fun tier_f ->
        let boxes =
          List.filter_map ids ~f:(fun id ->
            Heap_layout.box_of layouts ~tier_f ~id)
        in
        (* a pair counts as overlapping only when they properly intersect;
           boxes that share an edge are tiling, which is the point *)
        List.sum
          (module Int)
          (List.mapi boxes ~f:(fun index box -> index, box))
          ~f:(fun (index, (a : Heap_layout.Box.t)) ->
            List.sum
              (module Int)
              (List.filteri boxes ~f:(fun other (_ : Heap_layout.Box.t) ->
                 other > index))
              ~f:(fun (b : Heap_layout.Box.t) ->
                match
                  Float.( > ) (a.x +. a.w) (b.x +. 0.01)
                  && Float.( > ) (b.x +. b.w) (a.x +. 0.01)
                  && Float.( > ) (a.y +. a.h) (b.y +. 0.01)
                  && Float.( > ) (b.y +. b.h) (a.y +. 0.01)
                with
                | true -> 1
                | false -> 0)))
  in
  List.iter
    [ "map_nested"; "queue_of_queues"; "map_shared_payload" ]
    ~f:(fun fixture ->
      List.iter [ 1; 2; 3; 8; 16 ] ~f:(fun columns ->
        print_endline
          [%string
            "%{fixture} · %{columns#Int} across: %{overlaps ~fixture \
             ~columns#Int} overlapping pairs"]));
  [%expect
    {|
    map_nested · 1 across: 0 overlapping pairs
    map_nested · 2 across: 0 overlapping pairs
    map_nested · 3 across: 0 overlapping pairs
    map_nested · 8 across: 0 overlapping pairs
    map_nested · 16 across: 0 overlapping pairs
    queue_of_queues · 1 across: 0 overlapping pairs
    queue_of_queues · 2 across: 0 overlapping pairs
    queue_of_queues · 3 across: 0 overlapping pairs
    queue_of_queues · 8 across: 0 overlapping pairs
    queue_of_queues · 16 across: 0 overlapping pairs
    map_shared_payload · 1 across: 0 overlapping pairs
    map_shared_payload · 2 across: 0 overlapping pairs
    map_shared_payload · 3 across: 0 overlapping pairs
    map_shared_payload · 8 across: 0 overlapping pairs
    map_shared_payload · 16 across: 0 overlapping pairs
    |}]
;;

(* the columns slider's geometry: one column stacks, more than one packs the
   structures across, each column as wide as its own widest tree *)
let%expect_test "structures pack across into a grid" =
  let replay = replay_of_fixture "map_nested" in
  let roots, (_ : Heap_scene.Stats.t) =
    scene replay ~step:(Replay.length replay - 1)
  in
  print_endline [%string "%{List.length roots#Int} structures"];
  List.iter [ 1; 2; 3 ] ~f:(fun columns ->
    let layout =
      Heap_layout.compute
        roots
        ~tier:1
        ~shelves:(Heap_layout.shelves roots ~columns)
    in
    let heads =
      List.map layout.heads ~f:(fun (head : Heap_layout.Head.t) ->
        [%string
          "(%{Float.round_nearest head.x#Float},%{Float.round_nearest \
           head.y#Float})"])
      |> String.concat ~sep:" "
    in
    print_endline
      [%string
        "%{columns#Int} across: %{Float.round_nearest \
         layout.width#Float}×%{Float.round_nearest layout.height#Float} · \
         %{heads}"]);
  [%expect
    {|
    2 structures
    1 across: 241.×318. · (0.,0.) (0.,152.)
    2 across: 512.×166. · (0.,0.) (271.,0.)
    3 across: 512.×166. · (0.,0.) (271.,0.)
    |}]
;;

let%expect_test "a parent's box is centered over the spread of its children" =
  let replay = replay_of_fixture "map_nested" in
  let roots, (_ : Heap_scene.Stats.t) = scene replay ~step:1 in
  let layout =
    Heap_layout.compute
      roots
      ~tier:1
      ~shelves:(Heap_layout.shelves roots ~columns:1)
  in
  List.iter layout.placed ~f:(fun (placed : Heap_layout.Placed.t) ->
    let box = Map.find_exn layout.pos placed.id in
    print_endline
      [%string
        "%{placed.id} d%{placed.depth#Int} %{placed.edge_label} → \
         x=%{Float.round_nearest box.x#Float} y=%{Float.round_nearest \
         box.y#Float} w=%{Float.round_nearest box.w#Float}"]);
  [%expect
    {|
    1: d0  → x=0. y=30. w=241.
    2: d0  → x=0. y=182. w=241.
    2:0 d1 l → x=3. y=250. w=44.
    2:1 d1 r → x=63. y=250. w=175.
    |}]
;;

let%expect_test "zoom stops and tiers round-trip through each other" =
  List.iter [ 0.2; 0.45; 0.85; 1.; 1.38; 1.5; 2. ] ~f:(fun k ->
    let tier = Heap_layout.tier_for ~k in
    let back = Heap_layout.k_for_tier tier in
    print_endline
      [%string
        "k=%{sprintf \"%.2f\" k} → tier %{sprintf \"%.2f\" tier} → \
         k=%{sprintf \"%.2f\" back}"]);
  [%expect
    {|
    k=0.20 → tier 0.60 → k=0.20
    k=0.45 → tier 1.82 → k=0.45
    k=0.85 → tier 2.90 → k=0.85
    k=1.00 → tier 3.00 → k=0.90
    k=1.38 → tier 3.00 → k=0.90
    k=1.50 → tier 3.00 → k=0.90
    k=2.00 → tier 3.00 → k=0.90
    |}]
;;

let%expect_test "content switches tiers only once geometry has grown" =
  List.iter [ 0.; 0.5; 0.84; 0.85; 1.; 1.9; 3. ] ~f:(fun tier_f ->
    let split = Heap_layout.split tier_f in
    print_endline
      [%string
        "tier_f %{sprintf \"%.2f\" tier_f}: geometry \
         %{split.a#Int}→%{split.b#Int} at %{sprintf \"%.2f\" split.f} · \
         content %{Heap_layout.content_tier tier_f#Int}"]);
  [%expect
    {|
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
  let layouts = Heap_layout.all roots ~columns:1 in
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
  [%expect
    {|
    tier_f 1.00: w=241. h=34.
    tier_f 1.40: w=248. h=49.
    tier_f 1.85: w=257. h=65.
    |}]
;;
