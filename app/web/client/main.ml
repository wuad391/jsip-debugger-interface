open! Core
open Async_kernel
open Jsip_types
open Jsip_parsing
open Jsip_replay
open Jsip_web_components
open Jsip_web

(* The browser-side twin of [app/bin/main.ml]: fetch what the server holds —
   the dump, the sources it mentions, the optional heat profile — parse with
   exactly the readers the TUI uses, and start the app. Everything loads up
   front; a failure becomes a full-page error. *)

let load_replay () =
  match%map Async_js.Http.get "api/dump" with
  | Error error -> Error error
  | Ok contents ->
    Dump_reader.parse contents
    |> Or_error.map ~f:(fun parsed_info ->
      Replay.create (Call_stack.create ~parsed_info))
    |> Or_error.tag_s ~tag:[%message "cannot read dump"]
;;

(* every file the dump mentions, fetched and highlighted once up front; a
   missing file becomes a placeholder pane, not a failure *)
let load_sources replay =
  Deferred.List.map (Replay.files replay) ~how:`Sequential ~f:(fun path ->
    match%map Async_js.Http.get "api/source" ~arguments:[ "path", path ] with
    | Ok contents ->
      ( path
      , Ok
          (Source_model.Loaded.of_source_file
             (Source_file.of_lines (String.split_lines contents))) )
    | Error (_ : Error.t) ->
      ( path
      , Or_error.error_string
          [%string
            "%{path} is not under the server's -source-root — the dump's \
             paths resolve from the replayed program's root, so start \
             serve.exe there or pass -source-root DIR"] ))
  >>| String.Map.of_alist_exn
;;

let load_profile () =
  match%map Async_js.Http.get "api/heat" with
  (* the heat endpoint 404s when serve.exe was started without a
     [-perf-file]; uncolored frames, not an error *)
  | Error (_ : Error.t) -> None
  | Ok contents -> Heat_reader.parse contents |> Or_error.ok
;;

let load_dump_name () =
  match%map Async_js.Http.get "api/meta" with
  | Error (_ : Error.t) -> "dump"
  | Ok name -> (match String.strip name with "" -> "dump" | name -> name)
;;

let main () =
  let%bind replay = load_replay () in
  match replay with
  | Error error -> return (`Error error)
  | Ok replay ->
    (match Replay.length replay with
     | 0 ->
       return (`Error (Error.of_string "the dump has no events to replay"))
     | (_ : int) ->
       let%bind sources = load_sources replay in
       let%bind profile = load_profile () in
       let%map dump_name = load_dump_name () in
       `App (replay, sources, profile, dump_name))
;;

let () =
  Async_js.init ();
  don't_wait_for
    (match%map main () with
     | `Error error -> App.run_error ~error
     | `App (replay, sources, profile, dump_name) ->
       App.run ?profile ~replay ~sources ~dump_name ())
;;
