(** The web app: the replay wired to Bonsai state and the pane views.

    The browser-side twin of [Jsip_tui.App] — same model vocabulary
    (stepping, folds, accordion, filter, address order, flame drawer), with
    the heap outline replaced by {!Heap_widget}'s zoomable canvas. *)

open! Core
open Jsip_types
open Jsip_replay
open Jsip_web_components

(** Everything loaded and parsed up front, exactly like the TUI's [main]:
    the replay, each mentioned source file (a missing one renders as a
    placeholder pane), and the optional heat profile. *)
val run
  :  ?profile:Heat_profile.t
  -> replay:Replay.t
  -> sources:Source_model.Loaded.t Or_error.t String.Map.t
  -> dump_name:string
  -> unit
  -> unit

(** A full-page statement of why the app could not start — the browser's
    stand-in for the CLI printing an [Or_error] and exiting. *)
val run_error : error:Error.t -> unit
