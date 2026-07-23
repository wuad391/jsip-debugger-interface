open! Core
open Jsip_types
open Scanf

(** takes in section inside functions and parses it*)
(* looks like: "function_category:[%a]"*)
let parse_function function_input = 
  match sscanf line "%[^:]:[%[^]]]" 
  (fun function_category function_info -> match (function_category, function_info) with 
  | Ok (function_category, function_info) -> 
      match function_category with 
        | "function_name" -> Some ({function_name = function_info} : Function.Function_name.t)
        | "unnamed" -> Some ({function_content = function_info} : Function.Unnamed.t)
        | _ -> None
  | Error _ -> None
  )
;;

(** takes in a list of strings representing arguments and parses it*)
(* looks like: "[(LABEL:[%s],ARGUMENT:[%s]), ...]"*)
let parse_arguments args = 
  let parse_argument arg = 
    match sscanf line "(LABEL:[{%[^}]}{%[^}]}],ARGUMENT:[%[^]]])" 
    (fun arg_label label_info arg_info -> match (arg_label, label_info, arg_info) with 
    | Ok (arg_label, label_info, arg_info) -> 
        match function_category with 
          | "NO_LABEL" -> Some ({expression = arg_info} : Argument.No_label.t)
          | "LABELLED" -> Some ({label = label_info; expression = arg_info} : Argument.Labelled.t)
          | "OPTIONAL" -> Some ({label = label_info; expression = arg_info} : Argument.Optional.t)
          | _ -> None
    | Error _ -> None
    )
  in
;;

(** takes in a string representing a function call and parses it*)
(* looks like: "FUNCTION(...) ARGUMENTS(...) LOCATION(...)"*)
let parse_line line = 
  match sscanf line "FUNCTION(%[^)]) ARGUMENTS(%[^)]) LOCATION(%[^)])" 
  (fun function_name argument location -> match (function_name, arguments, location) with 
  | Ok (function_name, arguments, location) -> 
      parse_function function_name;
      parse_arguments String.split_on_char ',' arguments;
      parse_location location
  | Error _ -> ()
  )
;;

(* reads a file line by line until it is empty *)
let read_file file_path = 
  let channel = In_channel.open_text file_path in
  let rec read_until_empty = 
    match In_channel.input_line channel with 
    | Some line -> Printf.printf line 
    | None -> In_channel.close channel
  in  
  read_until_empty
;;