(** The one-row session bar across the top of the screen.

    App chip on the left, then the dump's name and the structure being
    replayed; the derived phase and the step counter on the right — the
    mockup's header strip. *)

open! Core
module View := Bonsai_term.View

(** [step] is 1-based for display. [phase] is the app's derived label
    (setup/descend/step/unwind); [structure] the {!Snapshot.Ds_type} chip. *)
val view
  :  width:int
  -> dump_name:string
  -> structure:string
  -> phase:string
  -> step:int
  -> total:int
  -> View.t
