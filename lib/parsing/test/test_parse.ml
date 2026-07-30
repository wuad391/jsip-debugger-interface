open! Core
open Jsip_types
open Jsip_parsing

(* the real dump of test/map_smoke.ml: two [M.add]s and an [M.remove] *)
let map_smoke_dump =
  {|{(event (id 1) (loc ((file_path ../test/map_smoke.ml) (line_number 5) (char_range (10 23)))) (fn (Function_name M.add)) (args ((No_label (expression (Unnamed "\"a\""))) (No_label (expression (Unnamed 1))) (No_label (expression (Unnamed m))))) (registry ((1 0x70d8d67f19f8))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x70d8d67f19f8) (block ((l (Int 0)) (v (String a)) (d (Int 1)) (r (Int 0)))) (children ()))))))
}{(event (id 2) (loc ((file_path ../test/map_smoke.ml) (line_number 6) (char_range (10 23)))) (fn (Function_name M.add)) (args ((No_label (expression (Unnamed "\"b\""))) (No_label (expression (Unnamed 2))) (No_label (expression (Unnamed m))))) (registry ((1 0x70d8d67f19f8) (2 0x70d8d67ee890))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x70d8d67ee890) (block ((l (Int 0)) (v (String a)) (d (Int 1)))) (children (((virtual_address 0x70d8d67ee8c0) (block ((l (Int 0)) (v (String b)) (d (Int 2)) (r (Int 0)))) (children ())))))))))
}{(event (id 3) (loc ((file_path ../test/map_smoke.ml) (line_number 7) (char_range (10 24)))) (fn (Function_name M.remove)) (args ((No_label (expression (Unnamed "\"a\""))) (No_label (expression (Unnamed m))))) (registry ((1 0x70d8d67f19f8) (2 0x70d8d67ee890) (3 0x70d8d67ee8c0))) (snapshot ((ds_type Map) (root_node ((virtual_address 0x70d8d67ee8c0) (block ((l (Int 0)) (v (String b)) (d (Int 2)) (r (Int 0)))) (children ()))))))
}|}
;;

let%expect_test "unsexp one event line" =
  let line = List.hd_exn (String.split_lines map_smoke_dump) in
  let line = String.chop_prefix_exn line ~prefix:"{" in
  print_s [%sexp (Dump_wire.of_string line : Dump_wire.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((id 1)
      (loc
       ((file_path ../test/map_smoke.ml) (line_number 5) (char_range (10 23))))
      (fn (Function_name M.add))
      (args
       ((No_label (expression (Unnamed "\"a\"")))
        (No_label (expression (Unnamed 1))) (No_label (expression (Unnamed m)))))
      (registry ((1 0x70d8d67f19f8)))
      (snapshot
       ((ds_type Map)
        (root_node
         ((virtual_address 0x70d8d67f19f8)
          (block ((l (Int 0)) (v (String a)) (d (Int 1)) (r (Int 0))))
          (children ())))))))
    |}]
;;

let%expect_test "read a whole dump into Call.Info values" =
  let file = "map_smoke_dump.txt" in
  Out_channel.write_all file ~data:map_smoke_dump;
  Dump_reader.read_until_empty file ~store_data:(fun info ->
    print_s [%sexp (info : Call.Info.t)]);
  [%expect
    {|
    ((depth 1) (id 1) (function_info (Function_name M.add))
     (location
      ((file_path ../test/map_smoke.ml) (line_number 5) (char_range (10 23))))
     (arguments
      ((No_label (expression (Unnamed "\"a\"")))
       (No_label (expression (Unnamed 1))) (No_label (expression (Unnamed m)))))
     (registry ((1 0x70d8d67f19f8)))
     (snapshot
      ((ds_type Map)
       (root_node
        ((virtual_address 0x70d8d67f19f8)
         (block ((l (Int 0)) (v (String a)) (d (Int 1)) (r (Int 0))))
         (children ()))))))
    ((depth 1) (id 2) (function_info (Function_name M.add))
     (location
      ((file_path ../test/map_smoke.ml) (line_number 6) (char_range (10 23))))
     (arguments
      ((No_label (expression (Unnamed "\"b\"")))
       (No_label (expression (Unnamed 2))) (No_label (expression (Unnamed m)))))
     (registry ((1 0x70d8d67f19f8) (2 0x70d8d67ee890)))
     (snapshot
      ((ds_type Map)
       (root_node
        ((virtual_address 0x70d8d67ee890)
         (block ((l (Int 0)) (v (String a)) (d (Int 1))))
         (children
          (((virtual_address 0x70d8d67ee8c0)
            (block ((l (Int 0)) (v (String b)) (d (Int 2)) (r (Int 0))))
            (children ())))))))))
    ((depth 1) (id 3) (function_info (Function_name M.remove))
     (location
      ((file_path ../test/map_smoke.ml) (line_number 7) (char_range (10 24))))
     (arguments
      ((No_label (expression (Unnamed "\"a\"")))
       (No_label (expression (Unnamed m)))))
     (registry ((1 0x70d8d67f19f8) (2 0x70d8d67ee890) (3 0x70d8d67ee8c0)))
     (snapshot
      ((ds_type Map)
       (root_node
        ((virtual_address 0x70d8d67ee8c0)
         (block ((l (Int 0)) (v (String b)) (d (Int 2)) (r (Int 0))))
         (children ()))))))
    |}]
;;

let%expect_test "a dump that does not return to depth 0 is rejected" =
  let file = "truncated_dump.txt" in
  let truncated =
    String.concat ~sep:"\n" (List.take (String.split_lines map_smoke_dump) 3)
  in
  Out_channel.write_all file ~data:(truncated ^ "\n}}\n");
  Expect_test_helpers_core.require_does_raise (fun () ->
    Dump_reader.read_until_empty
      file
      ~store_data:(ignore : Call.Info.t -> unit));
  [%expect {| (Failure "DUMP READER: Incorrect file ending!") |}]
;;

(* No stdlib Map/Set/Queue function takes a labelled argument, so a real
   dump of them only ever shows [No_label]; the wire and the derived
   reader carry labels all the same. *)
let%expect_test "labelled and optional arguments carry their labels" =
  let line =
    String.concat
      [ {|(event (id 7) (loc ((file_path t.ml) (line_number 4) |}
      ; {|(char_range (2 30)))) (fn (Function_name M.add)) |}
      ; {|(args ((Labelled (label key) (expression (Unnamed "\"a\""))) |}
      ; {|(Labelled (label data) (expression (Unnamed 1))) |}
      ; {|(Optional (label eq) (expression (Unnamed OMITTED))) |}
      ; {|(No_label (expression (Unnamed m))))) |}
      ; {|(registry ((7 0x1234))) (snapshot ((ds_type Map) (root_node |}
      ; {|((virtual_address 0x1234) (block ((v (String a)))) |}
      ; {|(children ()))))))|}
      ]
  in
  let wire = Dump_wire.of_string line |> Or_error.ok_exn in
  print_s [%sexp (wire.args : Argument.t list)];
  [%expect {|
    ((Labelled (label key) (expression (Unnamed "\"a\"")))
     (Labelled (label data) (expression (Unnamed 1)))
     (Optional (label eq) (expression (Unnamed OMITTED)))
     (No_label (expression (Unnamed m))))
    |}]
;;
