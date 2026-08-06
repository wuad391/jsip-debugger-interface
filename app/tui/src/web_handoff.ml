open! Core
open Async

type t =
  { dump_file : string
  ; source_root : string
  ; perf_file : string option
  }

(* the binary this process is running as — [Sys.executable_name] moved to
   core_unix's own library, and /proc answers the same question without a
   further dependency; argv.(0) is the fallback for the unlinked case *)
let self_exe () =
  match Core_unix.readlink "/proc/self/exe" with
  | path -> path
  | exception (_ : exn) -> (Sys.get_argv ()).(0)
;;

let file_exists path =
  match Core_unix.access path [ `Exists ] with
  | Ok () -> true
  | Error (_ : exn) -> false
;;

let serve_binary () =
  match Sys.getenv "JSIP_WEB_SERVE" with
  | Some path ->
    (match file_exists path with
     | true -> Ok path
     | false ->
       Or_error.error_s
         [%message "JSIP_WEB_SERVE points at nothing" (path : string)])
  | None ->
    (* app/bin/main.exe and app/web/server/serve.exe share a _build tree once
       the web branch is merged; before that this neighbour does not exist,
       and the error should say what is missing rather than shrug *)
    let beside =
      Filename.dirname (self_exe ())
      ^/ ".."
      ^/ "web"
      ^/ "server"
      ^/ "serve.exe"
    in
    (match file_exists beside with
     | true -> Ok beside
     | false ->
       Or_error.error_s
         [%message
           "the web twin is not built beside this binary — merge/build \
            app/web, or point JSIP_WEB_SERVE at a serve.exe"
             (beside : string)])
;;

(* a TCP bind probe: taking the port and letting it go again is the one test
   that agrees with what the server is about to do. The probe binds the
   loopback address the URL names. *)
let port_is_free port =
  let sock =
    Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 ()
  in
  Exn.protect
    ~finally:(fun () -> Core_unix.close sock)
    ~f:(fun () ->
      match
        Core_unix.bind
          sock
          ~addr:(ADDR_INET (Core_unix.Inet_addr.localhost, port))
      with
      | () -> true
      | exception
          Core_unix.Unix_error
            ((_ : Core_unix.Error.t), (_ : string), (_ : string)) ->
        false)
;;

let first_port = 8080
let port_attempts = 10

let free_port () =
  List.range first_port (first_port + port_attempts)
  |> List.find ~f:port_is_free
  |> Or_error.of_option
       ~error:
         (Error.create_s
            [%message
              "no free port for the web twin"
                (first_port : int)
                (port_attempts : int)])
;;

let url ~port = [%string "http://localhost:%{port#Int}"]

(* [$BROWSER] first: VS Code's remote terminals set it to a helper that
   forwards to the local browser, which is the one place "open localhost" is
   otherwise impossible. Unset, the empty command fails straight through to
   the desktop openers. Silence throughout — stdout belongs to the TUI's own
   drawing. *)
let browser_command ~port =
  let url = url ~port in
  [%string
    {|{ "$BROWSER" %{url} || xdg-open %{url} || open %{url}; } >/dev/null 2>&1|}]
;;

let shell command =
  don't_wait_for
    (Deferred.ignore_m
       (Deferred.Or_error.try_with (fun () ->
          Process.run ~prog:"/bin/sh" ~args:[ "-c"; command ] ())))
;;

let launch t =
  let open Or_error.Let_syntax in
  let%bind serve = serve_binary () in
  let%map port = free_port () in
  let flags =
    [ [ serve; "-dump-file"; t.dump_file ]
    ; [ "-source-root"; t.source_root ]
    ; (match t.perf_file with
       | None -> []
       | Some perf -> [ "-perf-file"; perf ])
    ; [ "-port"; Int.to_string port ]
    ]
    |> List.concat
    |> List.map ~f:Filename.quote
    |> String.concat ~sep:" "
  in
  (* detached under its own session so quitting the TUI leaves the twin
     serving; the beat before the browser gives the server its bind *)
  shell
    [%string
      {|setsid %{flags} >/dev/null 2>&1 </dev/null & sleep 0.3; %{browser_command ~port}|}];
  port
;;

let reopen ~port = shell (browser_command ~port)
