open! Core
open Jsip_types
open Scanf

(** takes in section inside functions and parses it*)
(* looks like: "function_category:[%a]"*)
let parse_function function_input = 
  match sscanf line "%s:[%s]" 
  (fun function_category function_info -> Ok (function_category, function_info)) with 
  | Ok (function_category, function_info) -> 
      match function_category with 
        | "function_name" -> Some ({function_name = function_info} : Function.Function_name.t)
        | "unnamed" -> Some ({function_content = function_info} : Function.Unnamed.t)
        | _ -> None
  | Error _ -> None
;;

(** takes in a list of strings representing arguments and parses it*)
(* looks like: "[(LABEL:[%s],ARGUMENT:[%s]), ...]"*)
let parse_arguments args = 
  
;;

(** takes in a string representing a function call and parses it*)
(* looks like: "FUNCTION(...) ARGUMENTS(...)"*)
let parse_line line = 
  match sscanf line "FUNCTION(%s) ARGUMENTS(%s)" 
  (fun function_name arguments -> Ok (function_name, arguments)) with 
  | Ok (function_name, arguments) -> 
      parse_function function_name;
      parse_arguments String.split_on_cjar ',' arguments
  | Error _ -> ()
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