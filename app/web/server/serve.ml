open! Core
open Async

(* The web interface's one command: serve the compiled client and the three
   inputs the TUI takes as flags. The client fetches [api/dump], the
   sources the dump mentions through [api/source?path=...], and the
   optional [api/heat]; parsing happens in the browser, through exactly the
   readers the TUI uses.

   Binds to localhost only: [api/source] resolves paths the dump recorded,
   which is the same local-file trust the TUI extends, and no wider. *)

let respond ?(content_type = "text/plain; charset=utf-8") ?(status = `OK) body =
  Cohttp_async.Server.respond_string
    ~status
    ~headers:(Cohttp.Header.of_list [ "Content-Type", content_type ])
    body
;;

let not_found reason = respond ~status:`Not_found reason

let respond_file path ~content_type =
  match%bind
    Monitor.try_with (fun () -> Reader.file_contents path)
  with
  | Ok contents -> respond ~content_type contents
  | Error (_ : exn) -> not_found [%string "cannot read %{path}"]
;;

(* the dump's paths resolve against the source root, exactly as the TUI
   resolves them; a path that climbs back out of nothing in particular is
   still confined to this machine, which is the trust a local debugger
   already has — but keep the resolution the TUI's, no more *)
let resolve_source ~source_root path =
  match Filename.is_absolute path with
  | true -> path
  | false -> source_root ^/ path
;;

let handler ~dump_file ~source_root ~perf_file ~body:(_ : Cohttp_async.Body.t) (_ : Socket.Address.Inet.t) request =
  let uri = Cohttp.Request.uri request in
  match Uri.path uri with
  | "/" | "/index.html" ->
    respond ~content_type:"text/html; charset=utf-8" Embedded_assets.index_html
  | "/main.bc.js" ->
    respond
      ~content_type:"text/javascript; charset=utf-8"
      Embedded_assets.main_bc_js
  | "/api/dump" -> respond_file dump_file ~content_type:"text/plain"
  | "/api/meta" -> respond (Filename.basename dump_file)
  | "/api/heat" ->
    (match perf_file with
     | None -> not_found "no -perf-file was given"
     | Some path -> respond_file path ~content_type:"text/plain")
  | "/api/source" ->
    (match Uri.get_query_param uri "path" with
     | None -> not_found "api/source wants ?path="
     | Some path ->
       respond_file
         (resolve_source ~source_root path)
         ~content_type:"text/plain")
  | (_ : string) -> not_found "unknown path"
;;

let main ~dump_file ~source_root ~perf_file ~port =
  (* fail fast on a dump that is not there — the browser saying 404 is a
     worse version of this message *)
  match%bind
    Monitor.try_with (fun () -> Reader.file_contents dump_file)
  with
  | Error (_ : exn) ->
    Deferred.Or_error.error_s
      [%message "cannot read dump" (dump_file : string)]
  | Ok (_ : string) ->
    let%bind (_ : (Socket.Address.Inet.t, int) Cohttp_async.Server.t) =
      Cohttp_async.Server.create
        ~on_handler_error:`Ignore
        (Tcp.Where_to_listen.bind_to Localhost (On_port port))
        (handler ~dump_file ~source_root ~perf_file)
    in
    printf "jsip web debugger → http://localhost:%d\n%!" port;
    let%map () = Deferred.never () in
    Ok ()
;;

let command =
  Command.async_or_error
    ~summary:"Serve the web interface for a -visual-replay dump"
    ~readme:(fun () ->
      "The browser twin of the terminal interface: the same replay with a \
       zoomable heap canvas. Serves the compiled client on localhost and \
       hands it the dump, the source files it mentions, and the optional \
       heat profile.\n\n\
      \  dune exec app/web/server/serve.exe -- -dump-file \
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
     and port =
       flag
         "-port"
         (optional_with_default 8080 int)
         ~doc:"PORT to listen on (default 8080)"
     in
     fun () -> main ~dump_file ~source_root ~perf_file ~port)
;;

let () = Command_unix.run command
