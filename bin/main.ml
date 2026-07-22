(** Sandbox entry point.

    Run with: dune exec bin/main.exe -- Ada *)

open! Core
open Jsip_parsing

let () =
  let path = "./dummy.txt"
  File_reader.read_file path
;;
