(* The stock ledger: a string-keyed map, grown one delivery at a time.
   Every tracked call in this file carries [inventory.ml] locations, so
   selecting one of its frames in the interface must swap the source
   pane to this file. *)

module M = Map.Make (String)

let initial () = M.add "apples" 12 (M.add "pears" 7 M.empty)
let restock item count stock = M.add item count stock
