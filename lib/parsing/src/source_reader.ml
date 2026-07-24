open! Core
open Jsip_types
open Scanf

let parse_line source_file line idx =
  let () = Array.set source_file.source !idx line in
  idx := !idx + 1
;;

let read_source source_file file_path =
  let idx = ref 0 in
  In_channel.with_file file_path ~f:(fun channel ->
    In_channel.iter_lines channel ~f:(fun line ->
      parse_line source_file line idx))
;;
