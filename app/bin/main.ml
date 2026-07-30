open! Core
open Async
open Jsip_types
open Jsip_parsing
open Jsip_replay
open Jsip_tui

let load_replay ~dump_file =
  Or_error.try_with (fun () ->
    let parsed_info = Queue.create () in
    Dump_reader.read_until_empty
      dump_file
      ~store_data:(Queue.enqueue parsed_info);
    Replay.create (Call_stack.create ~parsed_info))
  |> Or_error.tag_s ~tag:[%message "cannot read dump" (dump_file : string)]
;;

(* every file the dump mentions, loaded and highlighted once up front; a
   missing file becomes a placeholder pane, not a failure *)
let load_sources replay ~source_root =
  Replay.files replay
  |> List.map ~f:(fun path ->
    let resolved =
      match Filename.is_absolute path with
      | true -> path
      | false -> source_root ^/ path
    in
    ( path
    , Or_error.map
        (Source_reader.load resolved)
        ~f:Source_pane.Loaded.of_source_file ))
  |> String.Map.of_alist_exn
;;

let main ~dump_file ~source_root =
  let open Deferred.Or_error.Let_syntax in
  let%bind replay = Deferred.return (load_replay ~dump_file) in
  match Replay.length replay with
  | 0 ->
    Deferred.Or_error.error_s
      [%message "the dump has no events to replay" (dump_file : string)]
  | _ ->
    App.run
      ~dump_name:(Filename.basename dump_file)
      ~replay
      ~sources:(load_sources replay ~source_root)
;;

let command =
  Command.async_or_error
    ~summary:"Step through a -visual-replay dump of an OCaml program"
    ~readme:(fun () ->
      "Replays the log a jsip_debugger compiler writes under \
       -visual-replay: the call stack, the source position, and the walked \
       shape of every tracked structure, step by step.\n\n\
       Try the bundled demo:\n\n\
      \  dune exec app/bin/main.exe -- -dump-file demo/maps.dump")
    (let%map_open.Command dump_file =
       flag
         "-dump-file"
         (required string)
         ~doc:"FILE the compiler's -visual-replay dump"
     and source_root =
       flag
         "-source-root"
         (optional string)
         ~doc:
           "DIR directory the dump's source paths resolve against (default: \
            the dump's directory)"
     in
     fun () ->
       let source_root =
         Option.value source_root ~default:(Filename.dirname dump_file)
       in
       main ~dump_file ~source_root)
;;

let () = Command_unix.run command
