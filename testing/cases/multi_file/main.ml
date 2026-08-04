(* Drives the other two modules: the [Queue.fold] here is live while
   [Inventory.restock]'s adds fire over in inventory.ml, so the middle
   of this run has a call stack spanning two files — select the outer
   frame and the source pane must jump back to this one. *)

let () =
  let stock = Inventory.initial () in
  let arrivals = Basket.deliveries () in
  let stock =
    Queue.fold
      (fun acc item -> Inventory.restock item 1 acc)
      stock
      arrivals
  in
  ignore (Inventory.M.cardinal stock)
