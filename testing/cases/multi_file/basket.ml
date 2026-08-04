(* The day's deliveries, in arrival order — a queue built here so its
   events carry [basket.ml] locations. *)

let deliveries () =
  let q = Queue.create () in
  Queue.add "figs" q;
  Queue.add "plums" q;
  q
