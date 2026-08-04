open! Core
open Async
open Jsip_types
open Jsip_parsing
open Jsip_replay
open Jsip_tui

let load_replay ~dump_file =
  Dump_reader.read dump_file
  |> Or_error.map ~f:(fun parsed_info ->
    Replay.create (Call_stack.create ~parsed_info))
  |> Or_error.tag_s ~tag:[%message "cannot read dump" (dump_file : string)]
;;

(* every file the dump mentions, loaded and highlighted once up front; a
   missing file becomes a placeholder pane, not a failure — one that says
   where it looked and which flag moves the search, because the reader's own
   error is an exn sexp nobody should have to read in a 30-column pane *)
let load_sources replay ~source_root =
  Replay.files replay
  |> List.map ~f:(fun path ->
    let resolved =
      match Filename.is_absolute path with
      | true -> path
      | false -> source_root ^/ path
    in
    ( path
    , match Source_reader.load resolved with
      | Ok file -> Ok (Source_pane.Loaded.of_source_file file)
      | Error (_ : Error.t) ->
        Or_error.error_string
          [%string
            "%{path} is not at %{resolved} — the dump's paths resolve from \
             the replayed program's root, so run there or pass -source-root \
             DIR"] ))
  |> String.Map.of_alist_exn
;;

let main ~dump_file ~source_root ~perf_file =
  let open Deferred.Or_error.Let_syntax in
  let%bind replay = Deferred.return (load_replay ~dump_file) in
  (* loaded once up front like the sources; a bad heat file is this command's
     error, not a mystery of an uncolored pane *)
  let%bind profile =
    match perf_file with
    | None -> return None
    | Some path ->
      Deferred.return (Or_error.map (Heat_reader.load path) ~f:Option.some)
  in
  match Replay.length replay with
  | 0 ->
    Deferred.Or_error.error_s
      [%message "the dump has no events to replay" (dump_file : string)]
  | _ ->
    App.run
      ?profile
      ~dump_name:(Filename.basename dump_file)
      ~replay
      ~sources:(load_sources replay ~source_root)
      ()
;;

let command =
  Command.async_or_error
    ~summary:"Step through a -visual-replay dump of an OCaml program"
    ~readme:(fun () ->
      "Replays the log a jsip_debugger compiler writes under \
       -visual-replay: the call stack, the source position, and the walked \
       shape of every tracked structure, step by step.\n\n\
       The dumps record source paths relative to where the program was \
       compiled, so run from that directory (or point -source-root at it). \
       Try a bundled golden dump:\n\n\
      \  dune exec app/bin/main.exe -- -dump-file \
       testing/expected/map_nested.dump")
    (let%map_open.Command dump_file =
       flag
         "-dump-file"
         (required string)
         ~doc:"FILE the compiler's -visual-replay dump"
     and source_root =
       flag
         "-source-root"
         (optional_with_default "." string)
         ~doc:
           "DIR directory the dump's source paths resolve against (default: \
            the current directory)"
     and perf_file =
       flag
         "-perf-file"
         (optional string)
         ~doc:
           "FILE a heat profile (heat.sexp) the pipeline's perf stage \
            wrote; colors the call stack by each function's share of \
            sampled compute"
     in
     fun () -> main ~dump_file ~source_root ~perf_file)
;;

let () = Command_unix.run command
