open! Core
open Jsip_types
open Jsip_parsing

let contract =
  {|((version 1) (root_module Greet)
 (entries
  (((module_path (Greet)) (kind (Named shout)) (samples 8841))
   ((module_path (Greet))
    (kind
     (Anonymous (file_path greet.ml) (line_number 5) (char_range (27 33))))
    (samples 250)))))|}
;;

let%expect_test "a written contract file loads back intact" =
  Out_channel.write_all "heat.sexp" ~data:contract;
  let profile = Heat_reader.load "heat.sexp" |> Or_error.ok_exn in
  print_s [%sexp (profile : Heat_profile.t)];
  [%expect
    {|
    ((version 1) (root_module Greet)
     (entries
      (((module_path (Greet)) (kind (Named shout)) (samples 8841))
       ((module_path (Greet))
        (kind
         (Anonymous (file_path greet.ml) (line_number 5) (char_range (27 33))))
        (samples 250)))))
    |}]
;;

let%expect_test "a missing file is a tagged error, not a crash" =
  print_s
    [%sexp
      (Heat_reader.load "no_such_heat.sexp" : Heat_profile.t Or_error.t)];
  [%expect
    {|
    (Error
     (("Heat_reader: cannot load" no_such_heat.sexp)
      (Sys_error "no_such_heat.sexp: No such file or directory")))
    |}]
;;

let%expect_test "a garbled file is a tagged error, not a crash" =
  Out_channel.write_all "garbled.sexp" ~data:"((version 1) (oops))";
  let result = Heat_reader.load "garbled.sexp" in
  print_s [%sexp (Or_error.is_error result : bool)];
  [%expect {| true |}]
;;
