(** The one-row session bar across the top of the screen.

    App chip on the left, then the dump's name and the structure being
    replayed; the step counter on the right — the mockup's header strip. *)

open! Core
module View := Bonsai_term.View

(** [step] is 1-based for display; [structure] is the {!Snapshot.Ds_type}
    chip. *)
val view
  :  width:int
  -> dump_name:string
  -> structure:string
  -> step:int
  -> total:int
  -> View.t
