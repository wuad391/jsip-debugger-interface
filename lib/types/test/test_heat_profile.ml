open! Core
open Jsip_types

(* the contract sexp exactly as the pipeline's perf stage writes it (entries
   here echo a real capture over greet.ml, plus synthetic stdlib/anonymous
   entries to exercise every matching rule) *)
let profile =
  Heat_profile.t_of_sexp
    (Sexp.of_string
       {|
((version 1) (root_module Greet)
 (entries
  (((module_path (Greet)) (kind (Named shout)) (samples 8841))
   ((module_path (Greet)) (kind (Named generate_string)) (samples 3819))
   ((module_path (Greet)) (kind (Named generate_string2)) (samples 1912))
   ((module_path (Greet)) (kind (Named add)) (samples 137))
   ((module_path (Stdlib Map)) (kind (Named add)) (samples 63))
   ((module_path (Stdlib Map)) (kind (Named fold)) (samples 131))
   ((module_path (Stdlib String)) (kind (Named concat)) (samples 675))
   ((module_path (Greet))
    (kind
     (Anonymous (file_path greet.ml) (line_number 5) (char_range (27 33))))
    (samples 250)))))
|})
;;

let location ?(file_path = "greet.ml") ?(line_number = 1) () =
  Location.create ~file_path ~line_number ~char_range:(0, 10)
;;

let show ?location:(loc = location ()) function_info =
  let share = Heat_profile.share profile ~function_info ~location:loc in
  let rounded =
    Option.map share ~f:(Float.round_decimal ~decimal_digits:4)
  in
  print_s [%sexp (rounded : float option)]
;;

let%expect_test "total counts every entry" =
  print_s [%sexp (Heat_profile.total_samples profile : int)];
  [%expect {| 15828 |}]
;;

let%expect_test "an unqualified user function matches its entry" =
  show (Function_name "shout");
  [%expect {| (0.5586) |}]
;;

let%expect_test "a qualified stdlib name restricts by module path" =
  show (Function_name "Map.fold");
  [%expect {| (0.0083) |}]
;;

let%expect_test "an unresolvable alias qualifier is ignored" =
  (* "M" names nothing in the profile; candidates for [add] are the
     flambda2-specialized Greet.add and the generic Stdlib.Map.add, and both
     really are this function's compute: their samples sum *)
  show (Function_name "M.add");
  [%expect {| (0.0126) |}]
;;

let%expect_test "an unqualified name prefers the user's module" =
  (* [add] exists in Greet and in Stdlib.Map; unqualified, the user's own
     function wins *)
  show (Function_name "add");
  [%expect {| (0.0087) |}]
;;

let%expect_test "a name the profile has never sampled is neutral" =
  show (Function_name "uppercase_ascii");
  [%expect {| () |}];
  show (Function_name "Map.remove");
  [%expect {| () |}]
;;

let%expect_test "a lambda matches an anonymous entry on its line" =
  show ~location:(location ~line_number:5 ()) (Unnamed "(fun x -> x + 1)");
  [%expect {| (0.0158) |}];
  (* wrong line: neutral *)
  show ~location:(location ~line_number:6 ()) (Unnamed "(fun x -> x + 1)");
  [%expect {| () |}]
;;

(* the fallback behind [share]: no name and no lambda on the line, but the
   file is known and perf knows what that file cost *)
let%expect_test "a file's whole cost stands in when nothing else matches" =
  let show_file ?(file_path = "greet.ml") () =
    print_s
      [%sexp
        (Option.map
           (Heat_profile.file_share
              profile
              ~location:(location ~file_path ~line_number:41 ()))
           ~f:(Float.round_decimal ~decimal_digits:4)
         : float option)]
  in
  (* every [Greet] entry, named and anonymous, over the whole profile *)
  show_file ();
  [%expect {| (0.9451) |}];
  (* a file the profile never sampled stays neutral, not zero *)
  show_file ~file_path:"lib/other.ml" ();
  [%expect {| () |}];
  (* [share] itself still declines there — the fallback is the caller's to
     reach for *)
  show ~location:(location ~line_number:41 ()) (Unnamed "book.best_bid ()");
  [%expect {| () |}]
;;

let%expect_test "an empty profile is all neutral" =
  let empty =
    Heat_profile.t_of_sexp
      (Sexp.of_string "((version 1) (root_module Greet) (entries ()))")
  in
  print_s
    [%sexp
      (Heat_profile.share
         empty
         ~function_info:(Function_name "shout")
         ~location:(location ())
       : float option)];
  [%expect {| () |}]
;;

let%expect_test "extra top-level fields from a newer writer are tolerated" =
  let newer =
    Heat_profile.t_of_sexp
      (Sexp.of_string
         "((version 2) (root_module Greet) (entries ()) (came_later hi))")
  in
  print_s [%sexp (newer.version : int)];
  [%expect {| 2 |}]
;;
