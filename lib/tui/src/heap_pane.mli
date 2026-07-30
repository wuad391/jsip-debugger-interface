(** The heap pane: the current event's walked structure as a tree.

    Value fields sit inline in each node's line; pointer slots
    ({!Jsip_types.Snapshot.Ds_type.pointer_labels}) draw as labeled edges —
    [∅] where the runtime kept an immediate, a subtree where a block was
    walked. Nodes whose address first appeared at this step get the mockup's
    "freshly allocated" treatment. A [live] strip along the top shows the
    event's registry — every tracked structure still alive.

    {v
    live  1↦0x1a0  2↦0x2b0

    ● 0x2b0  v="a"  d=1  new
    ├─l→ ∅
    └─r→ ● 0x2b8  v="b"  d=2  new
         ├─l→ ∅
         └─r→ ∅
    v} *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** One drawn line, remembering which node it heads (edge and strip rows head
    none) so clicks can resolve to an address. *)
module Row : sig
  type t =
    { view : View.t
    ; address : Snapshot.Address.t option
    }
end

(** The rows [view] draws: the [live] registry strip, a blank, then the tree. *)
val rows
  :  snapshot:Snapshot.t
  -> registry:(int * Snapshot.Address.t) list
  -> new_addresses:Snapshot.Address.Set.t
  -> Row.t list

(** How far a scroll can go before the last row is already visible. *)
val scroll_limit : Row.t list -> height:int -> int

val view
  :  width:int
  -> height:int
  -> snapshot:Snapshot.t
  -> registry:(int * Snapshot.Address.t) list
  -> new_addresses:Snapshot.Address.Set.t
  -> scroll:int
  -> View.t

(** The node a click on pane-body row [row] lands on, mirroring [view]'s
    scrolling — the app jumps the replay to that node's allocation step. *)
val address_at
  :  snapshot:Snapshot.t
  -> registry:(int * Snapshot.Address.t) list
  -> new_addresses:Snapshot.Address.Set.t
  -> scroll:int
  -> height:int
  -> row:int
  -> Snapshot.Address.t option
