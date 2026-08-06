(** Handing the replay to the web twin: the [b web] chip's plumbing.

    The web interface (app/web, on its own branch while it settles) takes
    exactly the flags the terminal one does — [-dump-file], [-source-root],
    [-perf-file] — plus [-port], and serves the same replay at
    [http://localhost:PORT]. So converting a terminal session into a browser
    one is: start [serve.exe] on this session's own inputs, then open the
    URL. This module does both; {!App} owns when.

    [launch] is deliberately synchronous and quick — it picks the port and
    fires the two child processes without waiting on either — so it can run
    inside a key handler. The server is detached ([setsid], streams to
    /dev/null), so it outlives the terminal app: quitting the TUI must not
    tear the browser tab's data out from under it. *)

open! Core

type t =
  { dump_file : string (** as given to [-dump-file], not just the basename *)
  ; source_root : string
  ; perf_file : string option
  }

(** Start the twin on this session's inputs and open a browser at it.

    The server binary is [$JSIP_WEB_SERVE] if set, otherwise
    [app/web/server/serve.exe] beside this executable in [_build] — which
    exists once the web branch is merged and built, and does not before it;
    the error says so. The port is the first free one from 8080; the browser
    opener tries [$BROWSER] (which VS Code's remote terminals point at the
    local browser), then [xdg-open], then [open], and a failure there is
    silent — the URL is on the chip's caption in the README and the server is
    up either way.

    Returns the port it launched on. Errors are user-facing strings: no
    server binary, or no free port. *)
val launch : t -> int Or_error.t

(** Open the browser at an already-launched twin — [b] pressed again. *)
val reopen : port:int -> unit
