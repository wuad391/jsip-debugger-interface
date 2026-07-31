open! Core
open Jsip_types

let load file_path =
  Or_error.try_with (fun () ->
    Source_file.of_lines (In_channel.read_lines file_path))
  |> Or_error.tag_s ~tag:[%message "Source_reader: cannot load" file_path]
;;
