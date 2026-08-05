open! Core
open Jsip_types

let load file_path =
  Or_error.try_with (fun () ->
    Heat_profile.t_of_sexp (Sexp.load_sexp file_path))
  |> Or_error.tag_s ~tag:[%message "Heat_reader: cannot load" file_path]
;;

let parse contents =
  Or_error.try_with (fun () ->
    Heat_profile.t_of_sexp (Sexp.of_string contents))
  |> Or_error.tag_s ~tag:[%message "Heat_reader: cannot parse"]
;;
