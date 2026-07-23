open! Core
open Jsip_types
open In_channel
open Scanf

(* looks like: "function_category:[%a]"*)
let parse_function function_input =
  match
    sscanf_opt
      function_input
      "%[^:]:[%[^]]]"
      (fun function_category function_data ->
         function_category, function_data)
  with
  | Some (function_category, function_data) ->
    (match function_category with
     | "function_name" -> Some (Function_info.Function_name function_data)
     | "unnamed" -> Some (Function_info.Unnamed function_data)
     | _ -> None)
  | None -> None
;;

(* looks like: "[(LABEL:[%s],ARGUMENT:[%s]), ...]"*)
let parse_arguments args =
  let parse_argument arg =
    match
      sscanf_opt
        arg
        "LABEL:[{%[^}]}{%[^}]}],ARGUMENT:[%[^]]]"
        (fun arg_label label_info arg_info ->
           arg_label, label_info, arg_info)
    with
    | Some (arg_label, label_info, arg_info) ->
      (match arg_label with
       | "NO_LABEL" ->
         Some
           Argument.(
             No_label { expression = Function_info.Unnamed arg_info })
       | "LABELLED" ->
         Some
           Argument.(
             Labelled
               { label = label_info
               ; expression = Function_info.Unnamed arg_info
               })
       | "OPTIONAL" ->
         Some
           Argument.(
             Optional
               { label = label_info
               ; expression = Function_info.Unnamed arg_info
               })
       | _ -> None)
    | None -> None
  in
  let rec traverse_args acc unparsed_args =
    match unparsed_args with
    | [] -> acc
    | first_arg :: unparsed_args ->
      (match parse_argument first_arg with
       | Some arg -> traverse_args (arg :: acc) unparsed_args
       | None -> failwith "Internal Parsing error")
  in
  let rec reverse acc list =
    match list with [] -> acc | hd :: tl -> reverse (hd :: acc) tl
  in
  reverse [] (traverse_args [] args)
;;

(* takes in a string representing the location of the function call in the
   file and parses it *)
(* looks like: "LOCATION(File "./.tmp_files/tmp.ml", line 2, characters 24-56)"*)
let parse_location location =
  match
    sscanf_opt
      location
      "File %[^,], line %d, characters %d-%d"
      (fun file_path line_number char_range_start char_range_end ->
         file_path, line_number, char_range_start, char_range_end)
  with
  | Some (file_path, line_number, char_range_start, char_range_end) ->
    Some
      (Location.create
         ~file_path
         ~line_number
         ~char_range:(char_range_start, char_range_end))
  | None -> None
;;

let depth_change depth_update =
  let change = ref 0 in
  String.iter depth_update ~f:(fun char ->
    match char with
    | '{' -> change := !change + 1
    | '}' -> change := !change - 1
    | _ -> ());
  change
;;

(* looks like: "FUNCTION(...) ARGUMENTS(...) LOCATION(...)"*)

(** takes in a string representing a function call and parses it, pass in an
    external function to handle storing the data *)
let parse_line
  line
  (current_depth : int ref)
  (store_data :
    int -> Function_info.t -> Argument.t list -> Location.t -> unit)
  =
  match
    sscanf_opt
      line
      "%[^F]FUNCTION(%[^)]) ARGUMENTS(%[^)]) LOCATION(%[^)])"
      (fun depth_update function_name arguments location ->
         depth_update, function_name, arguments, location)
  with
  | Some (depth_update, function_name, arguments, location) ->
    (match
       ( depth_change depth_update
       , parse_function function_name
       , parse_arguments (String.split ~on:';' arguments)
       , parse_location location )
     with
     | depth_update, Some function_info, args_list, Some location ->
       current_depth := !current_depth + !depth_update;
       store_data !current_depth function_info args_list location
     | _ -> failwith "Internal Parsing error")
  | None -> failwith "ERROR file_reader = inputted parse line is incorrect"
;;

(* reads a file line by line until it is empty *)
let read_file
  file_path
  (store_data :
    int -> Function_info.t -> Argument.t list -> Location.t -> unit)
  =
  let current_depth = ref 0 in
  In_channel.with_file file_path ~f:(fun channel ->
    In_channel.iter_lines channel ~f:(fun line ->
      parse_line line current_depth store_data))
;;
