open! Core
open Jsip_types
open Jsip_parsing
open Jsip_replay
open Jsip_tui

(* render a view the way a colorless terminal would, for readable expects *)
let print_view ?(width = 60) ?(height = 14) view =
  let image = Bonsai_term.View.Private.notty_image view in
  let buffer = Buffer.create 1024 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (width, height) image;
  Buffer.contents buffer
  |> String.split_lines
  |> List.map ~f:String.rstrip
  |> List.rev
  |> List.drop_while ~f:String.is_empty
  |> List.rev
  |> String.concat ~sep:"\n"
  |> print_endline
;;

(* the same nested dump the replay tests replay: [M.of_list] calling [M.add],
   then [M.remove] back at depth 1 *)
let nested_dump =
  {|{(event (id 1) (loc ((file_path demo/maps.ml) (line_number 4) (char_range (10 32)))) (fn (Function_name M.of_list)) (args ((No_label (expression (Unnamed "[\"a\", 1]"))))) (registry ((1 0x1a0))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x1a0) (block ((v (String a)) (d (Int 1)))) (children ()))))))
{(event (id 2) (loc ((file_path demo/maps.ml) (line_number 5) (char_range (11 26)))) (fn (Function_name M.add)) (args ((No_label (expression (Unnamed "\"b\""))) (Labelled (label data) (expression (Unnamed 2))))) (registry ((1 0x1a0) (2 0x2b0))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x2b0) (block ((l (Int 0)) (v (String a)) (d (Int 1)))) (children (((virtual_address 0x2b8) (block ((l (Int 0)) (v (String b)) (d (Int 2)) (r (Int 0)))) (children ())))))))))
}}{(event (id 3) (loc ((file_path demo/maps.ml) (line_number 6) (char_range (11 25)))) (fn (Function_name M.remove)) (args ((No_label (expression (Unnamed "\"a\""))))) (registry ((1 0x1a0) (2 0x2b0) (3 0x2b8))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x2b8) (block ((v (String b)) (d (Int 2)))) (children ()))))))
}|}
;;

let replay =
  lazy
    (let file = "tui_dump.txt" in
     Out_channel.write_all file ~data:nested_dump;
     let parsed_info = Queue.create () in
     Dump_reader.read_until_empty
       file
       ~store_data:(Queue.enqueue parsed_info);
     Replay.create (Call_stack.create ~parsed_info))
;;

let%expect_test "stack pane rows, selection, and locations" =
  let replay = force replay in
  let { Replay.Step.frames; _ } = Replay.step_exn replay ~step:1 in
  print_view
    ~height:5
    (Stack_pane.view ~width:52 ~height:5 ~frames ~selected:1);
  [%expect
    {|
    ┌ CALL STACK ────────────────────────────── 2 live ┐
    │  M.of_list ["a", 1]                    maps.ml:4 │
    │ ▎  M.add "b" ~data:2                   maps.ml:5 │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: registry strip, tree edges, fresh nodes" =
  let replay = force replay in
  let { Replay.Step.call; new_addresses; _ } =
    Replay.step_exn replay ~step:1
  in
  print_view
    ~height:9
    (Heap_pane.view
       ~width:52
       ~height:9
       ~snapshot:call.info.snapshot
       ~registry:call.info.registry
       ~new_addresses
       ~scroll:0);
  [%expect
    {|
    ┌ HEAP ───────────────────── map · 2 nodes · 2 new ┐
    │ live  1↦0x1a0  2↦0x2b0                           │
    │                                                  │
    │ ● 0x2b0  v="a"  d=1  new                         │
    │ ├─l→ ∅                                           │
    │ └─r→ ● 0x2b8  v="b"  d=2  new                    │
    │      ├─l→ ∅                                      │
    │      └─r→ ∅                                      │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "heap pane: nothing fresh on the shared-root step" =
  let replay = force replay in
  let { Replay.Step.call; new_addresses; _ } =
    Replay.step_exn replay ~step:2
  in
  print_view
    ~height:6
    (Heap_pane.view
       ~width:52
       ~height:6
       ~snapshot:call.info.snapshot
       ~registry:call.info.registry
       ~new_addresses
       ~scroll:0);
  [%expect
    {|
    ┌ HEAP ───────────────────────────── map · 1 nodes ┐
    │ live  1↦0x1a0  2↦0x2b0  3↦0x2b8                  │
    │                                                  │
    │ ● 0x2b8  v="b"  d=2                              │
    │ ├─l→ ∅                                           │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "source pane: gutter, active line wash, callsite marker" =
  let source =
    Source_pane.Loaded.of_source_file
      (Source_file.of_lines
         [ "(* tiny demo *)"
         ; "module M = Map.Make (String)"
         ; ""
         ; "let t = M.of_list [\"a\", 1]"
         ; "let t' = M.add \"b\" 2 t"
         ; "let t'' = M.remove \"a\" t'"
         ])
  in
  print_view
    ~height:9
    (Source_pane.view
       ~width:52
       ~height:9
       ~file_label:"maps.ml"
       ~source:(Ok source)
       ~active_line:5
       ~callsite_line:(Some 4)
       ~char_range:(11, 26));
  [%expect
    {|
    ┌ SOURCE ─────────────────────── maps.ml · 6 lines ┐
    │    1 (* tiny demo *)                             │
    │    2 module M = Map.Make (String)                │
    │    3                                             │
    │ ▸  4 let t = M.of_list ["a", 1]                  │
    │ ▎  5 let t' = M.add "b" 2 t                      │
    │    6 let t'' = M.remove "a" t'                   │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "source pane: a missing file renders its error" =
  print_view
    ~height:5
    (Source_pane.view
       ~width:52
       ~height:5
       ~file_label:"gone.ml"
       ~source:(Or_error.error_string "no source loaded for gone.ml")
       ~active_line:1
       ~callsite_line:None
       ~char_range:(0, 0));
  [%expect
    {|
    ┌ SOURCE ─────────────────────── gone.ml · missing ┐
    │                                                  │
    │  no source loaded for gone.ml                    │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "footer: ticks mark past, current, future" =
  print_view
    ~height:3
    (Footer.view
       ~width:56
       ~step:1
       ~total:3
       ~playing:false
       ~status:"M.add \"b\" ~data:2 — maps.ml:5");
  [%expect
    {|
    ────────────────────────────────────────────────────────
     ━━━━━━━━━━━━━━━━━ ━━━━━━━━━━━━━━━━━ ━━━━━━━━━━━━━━━━━
     ◂ back  step ▸  ⏵ play  │ ▎ M.add "b" ~data:2 — maps.ml
    |}]
;;

let%expect_test "syntax spans" =
  let spans, depth =
    Syntax.line ~comment_depth:0 "let t' = M.add \"b\" 2 t (* nice *)"
  in
  print_s [%sexp (spans : (Syntax.Token.t * string) list)];
  print_s [%sexp (depth : int)];
  [%expect
    {|
    ((Keyword let) (Plain " ") (Plain t') (Plain " ") (Operator =) (Plain " ")
     (Uident M) (Operator .) (Plain add) (Plain " ") (String "\"b\"") (Plain " ")
     (Number 2) (Plain " ") (Plain t) (Plain " ") (Comment "(*")
     (Comment " nice *)"))
    0
    |}]
;;

let%expect_test "tick hit-testing round-trips" =
  let width = 26 in
  let total = 3 in
  let hits =
    List.init width ~f:(fun x -> Footer.step_at ~width ~total ~x)
    |> List.filter_map ~f:Fn.id
  in
  print_s [%sexp (List.dedup_and_sort hits ~compare : int list)];
  [%expect {| (0 1 2) |}]
;;
