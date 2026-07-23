(** Sandbox entry point.

    Run with: dune exec bin/main.exe -- Ada *)

open! Core
open Jsip_parsing

File_reader.read_until_empty "./dummy.txt"
