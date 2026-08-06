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

let show
  ?(folds = Int.Set.empty)
  ?(expanded = Int.Set.empty)
  ?selected
  replay
  ~step
  =
  let calls =
    Array.init (Replay.length replay) ~f:(fun step ->
      (Replay.step_exn replay ~step).call)
  in
  let heat = Array.map calls ~f:(fun (_ : Call.t) -> None) in
  let registered = Array.map calls ~f:(fun (_ : Call.t) -> None) in
  let live =
    List.map
      (Replay.step_exn replay ~step).frames
      ~f:(fun (frame : Call.t) -> snd frame.range)
  in
  let selected = Option.value selected ~default:(List.length live - 1) in
  let rows =
    Stack_rows.rows ~calls ~heat ~live ~selected ~folds ~expanded ~registered
  in
  List.iter rows ~f:(fun (row : Stack_rows.Row.t) ->
    let indent = String.make (2 * (row.depth - 1)) ' ' in
    let state =
      match row.state with
      | Stack_rows.State.Selected -> "●"
      | Live -> "○"
      | Dimmed -> " "
    in
    let glyph =
      match row.glyph with
      | Stack_rows.Glyph.Blank -> " "
      | Fold true | Run true -> "▸"
      | Fold false | Run false -> "▾"
    in
    let hidden =
      match row.hidden with
      | Stack_rows.Hidden.Nothing -> ""
      | Descendants count -> [%string " ⋯ %{count#Int}"]
      | Repeats count -> [%string " ⋯ ×%{count#Int}"]
    in
    let target = Sexp.to_string [%sexp (row.target : Stack_rows.Target.t)] in
    print_endline
      [%string
        "%{state} %{indent}%{glyph} %{row.fn} %{row.args}%{hidden} %{target}"])
;;

let%expect_test "the live chain is lit and everything else jumps" =
  let replay = replay_of_fixture "map_fold" in
  show replay ~step:2;
  [%expect
    {|
          M.add "b" 2 M.empty (Step 0)
      ▾ M.add "a" 1 (M.add "b" 2 M.empty) (Step 1)
    ●     M.add k (v * 2) acc (Frame 1)
          M.add k (v * 2) acc (Step 3)
    ○ ▾ M.fold (fun k v acc -> M.add k (v * 2) acc) m M.empty (Frame 0)
    |}]
;;

let%expect_test "folding a call hides its range behind a count" =
  let replay = replay_of_fixture "map_fold" in
  (* call 4 is the [M.fold] whose range spans the two inner adds *)
  show replay ~step:0 ~folds:(Int.Set.singleton 4);
  [%expect
    {|
    ●     M.add "b" 2 M.empty (Frame 1)
    ○ ▾ M.add "a" 1 (M.add "b" 2 M.empty) (Frame 0)
      ▸ M.fold (fun k v acc -> M.add k (v * 2) acc) m M.empty ⋯ 2 (Step 4)
    |}]
;;

let%expect_test "a run inside the live window never collapses" =
  (* map_spine_sharing is six identical [M.add] leaves, and at every step the
     live frame sits inside that run — so the protection keeps every row
     individually visible, which is the point of the protection *)
  let replay = replay_of_fixture "map_spine_sharing" in
  show replay ~step:(Replay.length replay - 1);
  [%expect
    {|
        ▸ M.add "f" 6 M.empty ⋯ ×4 (Step 0)
      ▾ M.add "j" 10 (seed ()) (Step 4)
    ●   M.add "g" 7 m (Frame 0)
    |}]
;;

(* run collapsing proper needs a run the eye is NOT in, which none of the
   small fixtures can stage — their live frame always lands inside the
   repeats — so these calls are built by hand: a loop's worth of identical
   leaves, then the call being watched *)
let synthetic_calls specs =
  Array.of_list
    (List.mapi specs ~f:(fun step (fn, depth) ->
       Call.create
         ~info:
           { Call.Info.depth
           ; id = 1
           ; function_info = Function_info.Function_name fn
           ; location =
               Location.create
                 ~file_path:"synthetic.ml"
                 ~line_number:(step + 1)
                 ~char_range:(0, 1)
           ; arguments = []
           ; registry = []
           ; ty = None
           ; binder = None
           ; scope = None
           ; snapshot = Snapshot.empty
           }
         ~range:(step, step)))
;;

let show_synthetic calls ~live ~selected ~expanded =
  let heat = Array.map calls ~f:(fun (_ : Call.t) -> None) in
  let registered = Array.map calls ~f:(fun (_ : Call.t) -> None) in
  let rows =
    Stack_rows.rows
      ~calls
      ~heat
      ~live
      ~selected
      ~folds:Int.Set.empty
      ~expanded
      ~registered
  in
  List.iter rows ~f:(fun (row : Stack_rows.Row.t) ->
    let hidden =
      match row.hidden with
      | Stack_rows.Hidden.Nothing -> ""
      | Descendants count -> [%string " ⋯ %{count#Int}"]
      | Repeats count -> [%string " ⋯ ×%{count#Int}"]
    in
    print_endline [%string "%{row.step#Int}: %{row.fn}%{hidden}"])
;;

let%expect_test "runs of identical leaves collapse past the threshold" =
  let calls =
    synthetic_calls
      (List.init 6 ~f:(fun (_ : int) -> "Queue.add", 1)
       @ [ "Book.publish", 1 ])
  in
  show_synthetic calls ~live:[ 6 ] ~selected:0 ~expanded:Int.Set.empty;
  [%expect {|
    0: Queue.add ⋯ ×6
    6: Book.publish
    |}]
;;

let%expect_test "an expanded run lists its members again" =
  let calls =
    synthetic_calls
      (List.init 6 ~f:(fun (_ : int) -> "Queue.add", 1)
       @ [ "Book.publish", 1 ])
  in
  let head =
    Stack_rows.run_head ~calls ~folds:Int.Set.empty ~live:[ 6 ] ~selected:0 3
  in
  print_s [%sexp (head : int option)];
  (match head with
   | None -> ()
   | Some head ->
     show_synthetic
       calls
       ~live:[ 6 ]
       ~selected:0
       ~expanded:(Int.Set.singleton head));
  [%expect
    {|
    (0)
    0: Queue.add
    1: Queue.add
    2: Queue.add
    3: Queue.add
    4: Queue.add
    5: Queue.add
    6: Book.publish
    |}]
;;
