(** Helpers for reading lines from an inputted directory path

*)
open! Core

(** Takes in a file path in the form of [./folder/file_name] and reads all the values until empty *)
val read_until_empty : string -> ()

