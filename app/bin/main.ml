open! Core
open Jsip_parsing

let main ~dump_file () =
  let call_stack = Call_stack.create ~size in
  File_reader.read_until_empty ~dump:dump_file ~call_stack
;;

let command =
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
;;
