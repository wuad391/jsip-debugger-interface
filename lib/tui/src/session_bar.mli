(** The one-row session bar across the bottom of the screen.

    App chip, then the dump's name and the structure being replayed — where
    the run is in the replay lives in the transport strip up top, not here. *)

open! Core
module View := Bonsai_term.View

(** [structure] is the {!Snapshot.Ds_type} chip. *)
val view : width:int -> dump_name:string -> structure:string -> View.t
