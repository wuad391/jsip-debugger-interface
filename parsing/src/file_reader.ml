open! Core
open Jsip_types
open In_channel
open Scanf

(** takes in section inside functions and parses it*)
(* looks like: "function_category:[%a]"*)
let parse_function function_input = 
  match sscanf_opt function_input "%[^:]:[%[^]]]" 
  (fun function_category function_info -> (function_category, function_info)) with 
  | Some (function_category, function_info) -> ( 
      match function_category with 
        | "function_name" -> Some (function_info : Function.Function_name.t)
        | "unnamed" -> Some ( function_info: Function.Unnamed.t)
        | _ -> None
      )
  | None -> None

(** takes in a list of strings representing arguments and parses it*)
(* looks like: "[(LABEL:[%s],ARGUMENT:[%s]), ...]"*)
let parse_arguments args = 
  let parse_argument arg = 
    match sscanf_opt arg "(LABEL:[{%[^}]}{%[^}]}],ARGUMENT:[%[^]]])" 
    (fun arg_label label_info arg_info -> (arg_label, label_info, arg_info)) with 
    | Some (arg_label, label_info, arg_info) -> 
        (match function_category with 
          | "NO_LABEL" -> Some ({expression = arg_info} : Argument.No_label.t)
          | "LABELLED" -> Some ({label = label_info; expression = arg_info} : Argument.Labelled.t)
          | "OPTIONAL" -> Some ({label = label_info; expression = arg_info} : Argument.Optional.t)
          | _ -> None)
    | None -> None
  in
  let rec traverse_args acc unparsed_args = match unparsed_args with 
    | [] -> acc
    | first_arg::unparsed_args -> match (parse_argument first_arg) with 
      | Some arg -> traverse_args arg::acc unparsed_args
      | None -> traverse_args acc unparsed_args
  in 
  let rec reverse acc list = match list with 
    | [] -> acc
    | hd::tl -> reverse (hd::acc) tl
  in
  reverse [] (traverse_args [] args)

(* takes in a string representing the location of the function call in the file and parses it*)
(* looks like: "LOCATION(File "./.tmp_files/tmp.ml", line 2, characters 24-56)"*)
let parse_location location = 
  match sscanf_opt location "LOCATION(File %[^,], line %d, characters %d-%d)"
  (fun file_path line_number char_range_start char_range_end ->  (file_path, line_number, char_range_start, char_range_end)) with 
  | Ok (file_path, line_number, char_range_start, char_range_end) -> Some ({
    file_path = file_path
    ; line_number = line_number
    ; char_range = char_range_start, char_range_end
  }:Location.t)
  | Error _ -> None
(** takes in a string representing a function call and parses it*)
(* looks like: "FUNCTION(...) ARGUMENTS(...) LOCATION(...)"*)
let parse_line line = 
  match sscanf_opt line "FUNCTION(%[^)]) ARGUMENTS(%[^)]) LOCATION(%[^)])" 
  (fun function_name arguments location -> (function_name, arguments, location)) with 
  | Some (function_name, arguments, location) -> (
      parse_function function_name;
      parse_arguments String.split_on_char ',' arguments;
      parse_location location)
  | None -> ()

(* reads a file line by line until it is empty *)
let read_file file_path = 
  let channel = In_channel.open_text file_path in
  let rec read_until_empty = 
    match In_channel.input_line channel with 
    | Some line -> Printf.printf line 
    | None -> In_channel.close channel
  in  
  read_until_empty