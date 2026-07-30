open! Core
open Jsip_types

let depth_change depth_update =
  let change = ref 0 in
  String.iter depth_update ~f:(fun char ->
    match char with
    | '{' -> change := !change + 1
    | '}' -> change := !change - 1
    | _ -> ());
  !change
;;

(* looks like:
   "[{(event (id 1) (loc ...) (fn ...) (args ...) (registry ...) (snapshot ...))" -- the {}]
   marker prefix carries the depth delta, the rest of the line is one event
   sexp that [Dump_wire] reads directly *)

(** takes in a string representing a function call and parses it, pass in an
    external function to handle storing the data *)
let parse_line
  line
  (current_depth : int ref)
  (store_data : Call.Info.t -> unit)
  =
  match String.index line '(' with
  | None ->
    (* a bare-marker line: only valid as the dump returning to depth 0 *)
    current_depth := !current_depth + depth_change line;
    (match !current_depth with
     | 0 -> ()
     | _ -> failwith "DUMP READER: Incorrect file ending!")
  | Some payload_start ->
    let markers = String.prefix line payload_start in
    let payload = String.drop_prefix line payload_start in
    current_depth := !current_depth + depth_change markers;
    let wire = Dump_wire.of_string payload |> Or_error.ok_exn in
    store_data
      ({ depth = !current_depth
       ; id = wire.id
       ; function_info = wire.fn
       ; location = wire.loc
       ; arguments = wire.args
       ; registry = wire.registry
       ; snapshot = wire.snapshot
       }
       : Call.Info.t)
;;

(* reads a file line by line until it is empty *)
let read_until_empty file_path ~store_data =
  let current_depth = ref 0 in
  In_channel.with_file file_path ~f:(fun channel ->
    In_channel.iter_lines channel ~f:(fun line ->
      parse_line line current_depth store_data))
;;
