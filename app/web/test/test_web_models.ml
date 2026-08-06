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

(* ── timeline ── *)

let%expect_test "segments keep bursts visible and clicks land on steps" =
  let density =
    Array.init 400 ~f:(fun step -> match step with 250 -> 1.0 | _ -> 0.05)
  in
  let segments = Timeline_model.segments ~density in
  let burst =
    Array.counti segments ~f:(fun (_ : int) value -> Float.( > ) value 0.5)
  in
  print_endline
    [%string
      "%{Array.length segments#Int} segments, %{burst#Int} carrying the \
       burst"];
  List.iter [ 0.; 0.5; 1. ] ~f:(fun fraction ->
    print_endline
      [%string
        "fraction %{sprintf \"%.1f\" fraction} → step \
         %{Timeline_model.step_of_fraction ~total:400 ~fraction#Int}"]);
  print_endline
    [%string
      "played at step 199: %{Timeline_model.played ~total:400 ~step:199 \
       ~segments:(Array.length segments)#Int}"];
  (* the cap sits at the end of the played span, and at step 0 it is already
     on screen — the strip has no other mark for the position *)
  List.iter [ 0; 199; 399 ] ~f:(fun step ->
    print_endline
      [%string
        "cursor at step %{step#Int}: %{Timeline_model.cursor ~total:400 \
         ~step ~segments:(Array.length segments)#Int}"]);
  [%expect
    {|
    160 segments, 1 carrying the burst
    fraction 0.0 → step 0
    fraction 0.5 → step 200
    fraction 1.0 → step 399
    played at step 199: 80
    cursor at step 0: 0
    cursor at step 199: 79
    cursor at step 399: 159
    |}]
;;

(* ── the heat gradient ── *)

let%expect_test "the heat gradient runs slate to orange, in both modes" =
  List.iter [ Theme.dark; Theme.light ] ~f:(fun theme ->
    let stops =
      List.map [ 0.; 0.35; 0.62; 0.82; 1. ] ~f:(Theme.heat_color theme)
    in
    print_endline
      [%string "%{theme.name}: %{String.concat stops ~sep:\" \"}"]);
  [%expect
    {|
    dark: #1b212a #2f6fb8 #7d8a5a #c9a24a #d4794f
    light: #e3e7ee #5b8fd0 #8f9b66 #c9a24a #c9703f
    |}]
;;

(* ── source rows ── *)

let%expect_test "source rows fold regions and mark the event's range" =
  let loaded =
    Source_model.Loaded.of_source_file
      (Source_file.of_lines
         [ "let add m ="
         ; "  M.add \"k\" 1 m"
         ; ""
         ; "let () ="
         ; "  ignore (add M.empty)"
         ])
  in
  print_s [%sexp (loaded.regions : (int * int) list)];
  let show rows =
    List.iter rows ~f:(fun (row : Source_model.Row.t) ->
      match row with
      | Source_model.Row.Folded_marker { start; stop; hides_active } ->
        print_endline
          [%string
            "  ⋯ %{start#Int}–%{stop#Int} hides_active %{hides_active#Bool}"]
      | Source_model.Row.Code
          { number; is_active; is_callsite; region_start; folded; spans } ->
        let marks =
          List.filter_opt
            [ (match is_active with true -> Some "active" | false -> None)
            ; (match is_callsite with
               | true -> Some "callsite"
               | false -> None)
            ; (match region_start with
               | true -> Some "region"
               | false -> None)
            ; (match folded with true -> Some "folded" | false -> None)
            ]
        in
        let text =
          List.map
            spans
            ~f:(fun ((_ : Jsip_parsing.Syntax.Token.t), text, marked) ->
              match marked with
              | true -> [%string "⟦%{text}⟧"]
              | false -> text)
          |> String.concat
        in
        print_endline
          [%string
            "%{number#Int} %{String.concat marks ~sep:\",\"}: %{text}"])
  in
  show
    (Source_model.rows
       loaded
       ~folds:Int.Set.empty
       ~active_line:2
       ~callsite_line:(Some 5)
       ~char_range:(2, 7));
  print_endline "— folded:";
  show
    (Source_model.rows
       loaded
       ~folds:(Int.Set.singleton 1)
       ~active_line:2
       ~callsite_line:None
       ~char_range:(0, 0));
  [%expect
    {|
    ((1 3) (4 5))
    1 region: let add m =
    2 active:   ⟦M⟧⟦.⟧⟦add⟧ "k" 1 m
    3 :
    4 region: let () =
    5 callsite:   ignore (add M.empty)
    — folded:
    1 region,folded: let add m =
      ⋯ 1–3 hides_active true
    4 region: let () =
    5 :   ignore (add M.empty)
    |}]
;;

(* ── flame bars ── *)

let%expect_test "flame bars tile their width, pool the sub-pixel tail" =
  let replay = replay_of_fixture "map_fold" in
  let calls =
    Array.init (Replay.length replay) ~f:(fun step ->
      (Replay.step_exn replay ~step).call)
  in
  let tree = Flame_tree.create ~calls ~profile:None in
  let live =
    Flame_math.live_path tree ~frames:(Replay.step_exn replay ~step:2).frames
  in
  let rows = Flame_math.bars tree ~zoom:[] ~width:400. ~live in
  List.iter rows ~f:(fun (row : Flame_math.Row.t) ->
    let segments =
      List.map row.segments ~f:(fun segment ->
        match segment with
        | Flame_math.Segment.Bar { label; x; width; lit; deepest; _ } ->
          let mark =
            match lit, deepest with
            | true, true -> "▏●"
            | true, false -> "▏"
            | false, (_ : bool) -> ""
          in
          [%string
            "%{mark}%{label}@%{Float.round_nearest \
             x#Float}+%{Float.round_nearest width#Float}"]
        | Flame_math.Segment.Pool { count; width; _ } ->
          [%string "+%{count#Int} (%{Float.round_nearest width#Float})"])
    in
    print_endline
      [%string
        "depth %{row.depth#Int}: %{String.concat segments ~sep:\" | \"}"]);
  print_endline
    (match Flame_math.heat_source tree with
     | `Compute -> "color = compute"
     | `Calls -> "color = calls");
  [%expect
    {|
    depth 0: M.add@0.+160. | ▏M.fold@160.+240.
    depth 1: M.add@0.+80. | ▏●M.add@160.+160.
    color = calls
    |}]
;;

let%expect_test "narrow children pool rather than vanish" =
  (* nine children of weight 1 into 24px: each would get 2.4px, under the 3px
     floor, so the whole row pools *)
  let rows =
    List.filter_map
      (Flame_math.bars
         (Flame_tree.create
            ~calls:
              (Array.of_list
                 (List.concat_map (List.init 9 ~f:Fn.id) ~f:(fun i ->
                    [ Call.create
                        ~info:
                          { Call.Info.depth = 2
                          ; id = 1
                          ; function_info =
                              Function_info.Function_name
                                [%string "f%{i#Int}"]
                          ; location =
                              Location.create
                                ~file_path:"synthetic.ml"
                                ~line_number:(i + 1)
                                ~char_range:(0, 1)
                          ; arguments = []
                          ; registry = []
                          ; ty = None
                          ; binder = None
                          ; scope = None
                          ; snapshot = Snapshot.empty
                          }
                        ~range:(i, i)
                    ])
                  @ [ Call.create
                        ~info:
                          { Call.Info.depth = 1
                          ; id = 1
                          ; function_info =
                              Function_info.Function_name "main"
                          ; location =
                              Location.create
                                ~file_path:"synthetic.ml"
                                ~line_number:99
                                ~char_range:(0, 1)
                          ; arguments = []
                          ; registry = []
                          ; ty = None
                          ; binder = None
                          ; scope = None
                          ; snapshot = Snapshot.empty
                          }
                        ~range:(0, 9)
                    ]))
            ~profile:None)
         ~zoom:[]
         ~width:24.
         ~live:[])
      ~f:(fun (row : Flame_math.Row.t) ->
        match row.depth with
        | 1 ->
          Some
            (List.map row.segments ~f:(fun segment ->
               match segment with
               | Flame_math.Segment.Bar { label; width; _ } ->
                 [%string "%{label}:%{Float.round_nearest width#Float}"]
               | Flame_math.Segment.Pool { count; width; _ } ->
                 [%string "+%{count#Int}:%{Float.round_nearest width#Float}"])
             |> String.concat ~sep:" ")
        | (_ : int) -> None)
  in
  List.iter rows ~f:print_endline;
  [%expect {| +9:12. |}]
;;
