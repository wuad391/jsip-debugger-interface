open! Core
open Jsip_types
open Scanf

let parse_line source_file line idx =
  let () = Array.set source_file.source !idx line in
  idx := !idx + 1
;;

let read_source source_file file_path =
  let channel = In_channel.open_text file _path in
  let idx = ref 0 in
  let rec read_until_empty () =
    match In_channel.input_line channel with
    | Some line ->
      parse_line source_file line idx;
      read_until_empty ()
    | None ->
      In_channel.close channel;
      ()
  in
  read_until_empty
;;
