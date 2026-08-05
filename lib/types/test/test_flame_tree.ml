open! Core
open Jsip_types

(* [Call_stack] reads only [depth]; [function_info] and [location] are what
   the flame tree keys on, and the rest is filler *)
let info ~depth ~function_info ~line_number : Call.Info.t =
  { depth
  ; id = depth
  ; function_info
  ; location =
      Location.create ~file_path:"t.ml" ~line_number ~char_range:(0, 1)
  ; arguments = []
  ; registry = []
  ; ty = None
  ; binder = None
  ; scope = None
  ; snapshot = Snapshot.empty
  }
;;

(* a frame, in the order the wire writes them: children before parents *)
let named ?(line = 1) name depth =
  Function_info.Function_name name, depth, line
;;

let lambda ?(line = 1) text depth = Function_info.Unnamed text, depth, line

let calls_of frames =
  let parsed_info =
    Queue.of_list
      (List.map frames ~f:(fun (function_info, depth, line_number) ->
         info ~depth ~function_info ~line_number))
  in
  (Call_stack.create ~parsed_info).call_order
;;

let tree ?profile frames =
  Flame_tree.create ~calls:(calls_of frames) ~profile
;;

let rec show_nodes nodes ~indent =
  List.iter nodes ~f:(fun (node : Flame_tree.Node.t) ->
    print_endline
      [%string
        "%{String.make indent ' '}%{Flame_tree.Key.display node.key} \
         inclusive=%{node.inclusive#Int} calls=%{node.calls#Int} \
         first=%{node.first_step#Int}"];
    show_nodes node.children ~indent:(indent + 2))
;;

let show (t : Flame_tree.t) =
  print_endline [%string "total_events=%{t.total_events#Int}"];
  show_nodes t.roots ~indent:0
;;

let round = Option.map ~f:(Float.round_decimal ~decimal_digits:4)

let%expect_test "a nested dump folds into one bar per path" =
  (* the same depths test_types.ml walks, so the two read together *)
  show
    (tree
       [ named "a" 1; named "b" 2; named "c" 3; named "d" 2; named "e" 1 ]);
  [%expect
    {|
    total_events=5
    a inclusive=1 calls=1 first=0
    e inclusive=4 calls=1 first=4
      b inclusive=1 calls=1 first=1
      d inclusive=2 calls=1 first=3
        c inclusive=1 calls=1 first=2
    |}]
;;

let%expect_test "repeated siblings pool into one bar" =
  show (tree [ named "add" 2; named "add" 2; named "add" 2; named "main" 1 ]);
  [%expect
    {|
    total_events=4
    main inclusive=4 calls=1 first=3
      add inclusive=3 calls=3 first=0
    |}]
;;

let%expect_test "the same function under two parents stays two bars" =
  show (tree [ named "f" 2; named "a" 1; named "f" 2; named "b" 1 ]);
  [%expect
    {|
    total_events=4
    a inclusive=2 calls=1 first=1
      f inclusive=1 calls=1 first=0
    b inclusive=2 calls=1 first=3
      f inclusive=1 calls=1 first=2
    |}]
;;

let%expect_test "recursion stacks into a tower, one bar per depth" =
  let t = tree [ named "fib" 3; named "fib" 2; named "fib" 1 ] in
  show t;
  (* the profile charges every depth to one symbol, so [Metrics.calls] — cost
     per call's denominator — counts the whole tower *)
  let metrics = Map.find_exn t.functions (Named "fib") in
  print_s [%sexp (metrics.calls : int)];
  [%expect
    {|
    total_events=3
    fib inclusive=3 calls=1 first=2
      fib inclusive=2 calls=1 first=1
        fib inclusive=1 calls=1 first=0
    3
    |}]
;;

let%expect_test "widths tile: a bar is its own calls plus its children" =
  let rec check (node : Flame_tree.Node.t) =
    let children =
      List.sum
        (module Int)
        node.children
        ~f:(fun (child : Flame_tree.Node.t) -> child.inclusive)
    in
    (match node.inclusive = node.calls + children with
     | true -> ()
     | false ->
       print_endline
         [%string "MISMATCH at %{Flame_tree.Key.display node.key}"]);
    List.iter node.children ~f:check
  in
  let t =
    tree
      [ named "c" 3
      ; named "b" 2
      ; named "b" 2
      ; named "d" 2
      ; named "a" 1
      ; named "e" 2
      ; named "f" 1
      ]
  in
  List.iter t.roots ~f:check;
  let roots =
    List.sum (module Int) t.roots ~f:(fun (root : Flame_tree.Node.t) ->
      root.inclusive)
  in
  print_endline
    [%string "roots=%{roots#Int} total_events=%{t.total_events#Int}"];
  [%expect {| roots=7 total_events=7 |}]
;;

let%expect_test "several top-level calls to one function make one root" =
  show (tree [ named "m" 1; named "m" 1; named "m" 1 ]);
  [%expect {|
    total_events=3
    m inclusive=3 calls=3 first=0
    |}]
;;

let%expect_test "a single top-level call is the only root" =
  show (tree [ named "x" 2; named "y" 2; named "top" 1 ]);
  [%expect
    {|
    total_events=3
    top inclusive=3 calls=1 first=2
      x inclusive=1 calls=1 first=0
      y inclusive=1 calls=1 first=1
    |}]
;;

let%expect_test "an empty dump gives an empty forest" =
  let t = tree [] in
  show t;
  print_s [%sexp (Map.length t.functions : int)];
  [%expect {|
    total_events=0
    0
    |}]
;;

let%expect_test "siblings come in name order, whatever their size" =
  (* [wide] runs three calls deep and [also] one, but the x-axis is
     alphabetical: two captures of the same program lay out identically, so
     they can be read side by side *)
  show
    (tree
       [ named "inner" 3
       ; named "wide" 2
       ; named "narrow" 2
       ; named "also" 2
       ; named "root" 1
       ]);
  [%expect
    {|
    total_events=5
    root inclusive=5 calls=1 first=4
      also inclusive=1 calls=1 first=3
      narrow inclusive=1 calls=1 first=2
      wide inclusive=2 calls=1 first=1
        inner inclusive=1 calls=1 first=0
    |}]
;;

let%expect_test "a named function pools across its call sites" =
  (* the profile matches a name regardless of where it was called, so
     splitting per site would spread one number over several bars *)
  show (tree [ named ~line:10 "f" 2; named ~line:20 "f" 2; named "main" 1 ]);
  [%expect
    {|
    total_events=3
    main inclusive=3 calls=1 first=2
      f inclusive=2 calls=2 first=0
    |}]
;;

let%expect_test "lambdas split by site but pool within one" =
  show
    (tree
       [ lambda ~line:10 "(fun x -> x)" 2
       ; lambda ~line:20 "(fun x -> x)" 2
       ; lambda ~line:20 "(fun x -> x)" 2
       ; named "main" 1
       ]);
  [%expect
    {|
    total_events=4
    main inclusive=4 calls=1 first=3
      (fun x -> x) inclusive=1 calls=1 first=0
      (fun x -> x) inclusive=2 calls=2 first=1
    |}]
;;

(* one function that is called once and burns nine tenths of the compute, and
   one that is called forty times for a tenth of it: the two shapes of
   bottleneck, and the pair cost-per-call exists to tell apart *)
let bench_profile =
  Heat_profile.t_of_sexp
    (Sexp.of_string
       {|
((version 1) (root_module Bench)
 (entries
  (((module_path (Bench)) (kind (Named shout)) (samples 9000))
   ((module_path (Bench)) (kind (Named add)) (samples 1000)))))
|})
;;

let bench_frames =
  List.init 40 ~f:(fun (_ : int) -> named "add" 2)
  @ [ named "shout" 2; named "main" 1 ]
;;

let show_metrics (t : Flame_tree.t) =
  List.iter
    (Flame_tree.by_cost_per_call t)
    ~f:(fun (key, (metrics : Flame_tree.Metrics.t)) ->
      let name = Flame_tree.Key.display key in
      let calls = metrics.calls in
      let share = round metrics.share in
      let relative = round metrics.relative_cost in
      print_s
        [%message
          name (calls : int) (share : float option) (relative : float option)])
;;

let%expect_test "cost per call tells the slow function from the chatty one" =
  show_metrics (tree ~profile:bench_profile bench_frames);
  [%expect
    {|
    (shout (calls 1) (share (0.9)) (relative (36.9)))
    (add (calls 40) (share (0.1)) (relative (0.1025)))
    |}]
;;

let%expect_test "an unmatched function is neutral and does not skew the rest"
  =
  (* [helper] is absent from the profile: it must not enter the average,
     which would otherwise move every other function's relative cost *)
  let t =
    tree
      ~profile:bench_profile
      (List.init 10 ~f:(fun (_ : int) -> named "helper" 2) @ bench_frames)
  in
  show_metrics t;
  let helper = Map.find_exn t.functions (Named "helper") in
  print_s [%sexp (helper : Flame_tree.Metrics.t)];
  [%expect
    {|
    (shout (calls 1) (share (0.9)) (relative (36.9)))
    (add (calls 40) (share (0.1)) (relative (0.1025)))
    ((calls 10) (share ()) (cost_per_call ()) (relative_cost ()))
    |}]
;;

let%expect_test "with no profile every metric is neutral but calls counts" =
  let t = tree bench_frames in
  print_s
    [%sexp (Map.find_exn t.functions (Named "add") : Flame_tree.Metrics.t)];
  print_s
    [%sexp
      (Flame_tree.by_cost_per_call t
       : (Flame_tree.Key.t * Flame_tree.Metrics.t) list)];
  [%expect
    {|
    ((calls 40) (share ()) (cost_per_call ()) (relative_cost ()))
    ()
    |}]
;;

let%expect_test "prorated share splits a function across its bars" =
  (* [add] runs once under [a] and three times under [b]; the two bars take a
     quarter and three quarters of its share, and sum back to it *)
  let t =
    tree
      ~profile:bench_profile
      [ named "add" 2
      ; named "a" 1
      ; named "add" 2
      ; named "add" 2
      ; named "add" 2
      ; named "b" 1
      ]
  in
  let bars =
    List.concat_map t.roots ~f:(fun (root : Flame_tree.Node.t) ->
      List.map root.children ~f:(fun child ->
        Flame_tree.Key.display root.key, Flame_tree.prorated_share t child))
  in
  List.iter bars ~f:(fun (parent, share) ->
    print_s [%message (parent : string) (round share : float option)]);
  let total =
    List.sum (module Float) bars ~f:(fun ((_ : string), share) ->
      Option.value share ~default:0.)
  in
  print_s [%sexp (Float.round_decimal total ~decimal_digits:4 : float)];
  [%expect
    {|
    ((parent a) ("round share" (0.025)))
    ((parent b) ("round share" (0.075)))
    0.1
    |}]
;;

let%expect_test "the lit path follows the replay's live stack" =
  let frames =
    [ named "a" 1; named "b" 2; named "c" 3; named "d" 2; named "e" 1 ]
  in
  let calls = calls_of frames in
  let t = Flame_tree.create ~calls ~profile:None in
  let stack =
    Call_stack.create
      ~parsed_info:
        (Queue.of_list
           (List.map frames ~f:(fun (function_info, depth, line_number) ->
              info ~depth ~function_info ~line_number)))
  in
  List.iter
    (List.init (Array.length calls) ~f:Fn.id)
    ~f:(fun step ->
      let lit =
        Flame_tree.path t ~frames:(Call_stack.frames_at stack ~step)
        |> List.map ~f:(fun (node : Flame_tree.Node.t) ->
          Flame_tree.Key.display node.key)
        |> String.concat ~sep:" > "
      in
      print_endline [%string "step %{step#Int}: %{lit}"]);
  [%expect
    {|
    step 0: a
    step 1: e > b
    step 2: e > d > c
    step 3: e > d
    step 4: e
    |}]
;;

let%expect_test "the lit path stops where the frames leave the tree" =
  (* frames from another run: [a > b] is shared, [zzz] is not, so the path is
     the prefix that resolves rather than a raise or a gap *)
  let t = tree [ named "c" 3; named "b" 2; named "a" 1 ] in
  let stranger =
    Call_stack.create
      ~parsed_info:
        (Queue.of_list
           (List.map
              [ named "zzz" 3; named "b" 2; named "a" 1 ]
              ~f:(fun (function_info, depth, line_number) ->
                info ~depth ~function_info ~line_number)))
  in
  let lit =
    Flame_tree.path t ~frames:(Call_stack.frames_at stranger ~step:0)
    |> List.map ~f:(fun (node : Flame_tree.Node.t) ->
      Flame_tree.Key.display node.key)
  in
  print_s [%sexp (lit : string list)];
  [%expect {| (a b) |}]
;;

let%expect_test "find resolves a zoom path" =
  let t = tree [ named "c" 3; named "b" 2; named "a" 1 ] in
  let show path =
    print_s
      [%sexp
        (Option.map (Flame_tree.find t ~path) ~f:(fun node ->
           Flame_tree.Key.display node.key)
         : string option)]
  in
  show [ Named "a"; Named "b" ];
  show [ Named "a"; Named "nope" ];
  show [];
  [%expect {|
    (b)
    ()
    ()
    |}]
;;

let%expect_test "the whole tree's sexp" =
  print_s
    [%sexp (tree [ named "b" 2; named "b" 2; named "a" 1 ] : Flame_tree.t)];
  [%expect
    {|
    ((roots
      (((key (Named a)) (inclusive 3) (calls 1) (first_step 2)
        (children
         (((key (Named b)) (inclusive 2) (calls 2) (first_step 0) (children ())))))))
     (total_events 3)
     (functions
      (((Named a) ((calls 1) (share ()) (cost_per_call ()) (relative_cost ())))
       ((Named b) ((calls 2) (share ()) (cost_per_call ()) (relative_cost ()))))))
    |}]
;;
