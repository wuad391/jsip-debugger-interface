open! Core
open Jsip_types
open Jsip_parsing

let main file_path =
  (* initially parse all the content *)
  let temp_queue = Queue.create () in
  let store_temp_state (temp_data : Call.Info.t) =
    Queue.enqueue temp_queue temp_data
  in
  Dump_reader.read_until_empty file_path ~store_data:store_temp_state;
  (* create call_stack *)
  let call_stack = Call_stack.create temp_queue in
  ()
;;

(*=let command =
  Command.async
    ~summary:"Bonsai_term visual debugger"
    (let%map_open.Command dump_file =
       flag
         "-dump_file"
         (required string)
         ~doc:"The dump file from the compiler with flag -visual-replay set"
         string
     in
     fun () -> main ~dump_file)
  |> Command_unix.run
;;*)
