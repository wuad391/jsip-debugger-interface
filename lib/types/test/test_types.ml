open! Core
open Jsip_types

(* build a minimal [Call.Info.t] — only [depth] matters to the range
   computation; everything else is filler *)
let info ~depth ~name : Call.Info.t =
  { depth
  ; id = depth
  ; function_info = Function_info.Function_name name
  ; location =
      Location.create ~file_path:"t.ml" ~line_number:depth ~char_range:(0, 1)
  ; arguments = []
  ; registry = []
  ; ty = None
  ; snapshot = Snapshot.empty
  }
;;

let stack_of_depths named_depths =
  let calls =
    Queue.of_list
      (List.map named_depths ~f:(fun (name, depth) -> info ~depth ~name))
  in
  Call_stack.create ~parsed_info:calls
;;

let name (call : Call.t) =
  match call.info.function_info with Function_name s | Unnamed s -> s
;;

let show t =
  Array.iteri t.Call_stack.call_order ~f:(fun i (call : Call.t) ->
    let lo, hi = call.range in
    print_endline
      [%string
        "%{i#Int}: %{name call} depth=%{call.info.depth#Int} \
         range=(%{lo#Int}, %{hi#Int})"])
;;

let%expect_test "call ranges over a nested dump" =
  (* shape: a { b { c } d } e  — depths 1 2 3 2 1 *)
  let t = stack_of_depths [ "a", 1; "b", 2; "c", 3; "d", 2; "e", 1 ] in
  show t;
  [%expect
    {|
    0: a depth=1 range=(0, 3)
    1: b depth=2 range=(1, 2)
    2: c depth=3 range=(2, 2)
    3: d depth=2 range=(3, 3)
    4: e depth=1 range=(4, 4)
    |}]
;;

let%expect_test "frames_at rebuilds the stack for each step" =
  let t = stack_of_depths [ "a", 1; "b", 2; "c", 3; "d", 2; "e", 1 ] in
  List.iter
    (List.init (Call_stack.length t) ~f:Fn.id)
    ~f:(fun step ->
      let frames =
        Call_stack.frames_at t ~step
        |> List.map ~f:name
        |> String.concat ~sep:" > "
      in
      print_endline [%string "step %{step#Int}: %{frames}"]);
  [%expect
    {|
    step 0: a
    step 1: a > b
    step 2: a > b > c
    step 3: a > d
    step 4: e
    |}]
;;

let%expect_test "a dump that skips depths on the way back up" =
  (* depth can fall by more than one between consecutive events *)
  let t = stack_of_depths [ "a", 1; "b", 2; "c", 3; "d", 1 ] in
  show t;
  [%expect
    {|
    0: a depth=1 range=(0, 2)
    1: b depth=2 range=(1, 2)
    2: c depth=3 range=(2, 2)
    3: d depth=1 range=(3, 3)
    |}]
;;

let%expect_test "an empty dump makes an empty call stack" =
  let t = stack_of_depths [] in
  print_s [%sexp (Call_stack.length t : int)];
  [%expect {| 0 |}]
;;

let%expect_test "type_info display picks the roles a human reads" =
  let show printed params =
    print_endline (Type_info.display { Type_info.printed; params })
  in
  show "int M.t" [ "key", "string"; "data", "int" ];
  show "S.t" [ "elt", "int" ];
  show "(int -> int) Queue.t" [ "elt", "int -> int" ];
  (* no role resolved (e.g. the map's module was a functor parameter): the
     printed type stands alone *)
  show "int M.t" [];
  [%expect
    {|
    ⟨string ⇒ int⟩
    ⟨int⟩
    ⟨int -> int⟩
    ⟨int M.t⟩
    |}]
;;

let%expect_test "hostile payload strings display escaped" =
  List.iter
    [ "plain"
    ; {|quote"and\back|}
    ; "newline\nand\ttab"
    ; "nul\000and\255high"
    ]
    ~f:(fun s -> print_endline (Snapshot.Block.display (String s)));
  [%expect
    {|
    "plain"
    "quote\"and\\back"
    "newline\nand\ttab"
    "nul\000and\255high"
    |}]
;;
