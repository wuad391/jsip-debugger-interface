(** The heap pane: the current event's walked structure as a tree of the
    mockup's node cards.

    Each node draws as a bordered card — its meaning up top ([{"a" ↦ 2}] for
    a map binding, the element for a set, [length n] for a queue root, the
    content for a cell, the joined positions for a walked value block), the
    tail of its address in small type below. Pointer slots (the emitter's
    masked-layout contract, {!Jsip_types.Snapshot.Ds_type.masked_labels})
    hang off the card as labeled edges — a subtree where a block was walked,
    [∅] where an interior node's slot is empty; leaves keep their empty slots
    to themselves. Cards allocated at this step get the fresh border and a
    [new] chip. Queue cells' numeric wire labels print as [v]/[next].

    {v
    ┌───────────────┐
    │ "a" ↦ 2   new │
    │ 0x…a278       │
    └───────────────┘
    ├─l→ ∅
    └─r→ ┌──────────────┐
         │ "b" ↦ 4  new │
         │ 0x…a2a8      │
         └──────────────┘
    v} *)

open! Core
open Jsip_types
module View := Bonsai_term.View

(** One drawn line, remembering which node it belongs to (edge rows belong to
    none) so clicks can resolve to an address. *)
module Row : sig
  type t =
    { view : View.t
    ; address : Snapshot.Address.t option
    }
end

val rows
  :  snapshot:Snapshot.t
  -> new_addresses:Snapshot.Address.Set.t
  -> Row.t list

(** How far a scroll can go before the last row is already visible. *)
val scroll_limit : Row.t list -> height:int -> int

val view
  :  width:int
  -> height:int
  -> snapshot:Snapshot.t
  -> new_addresses:Snapshot.Address.Set.t
  -> scroll:int
  -> View.t

(** The node a click on pane-body row [row] lands on, mirroring [view]'s
    scrolling — the app jumps the replay to that node's allocation step. *)
val address_at
  :  snapshot:Snapshot.t
  -> new_addresses:Snapshot.Address.Set.t
  -> scroll:int
  -> height:int
  -> row:int
  -> Snapshot.Address.t option
