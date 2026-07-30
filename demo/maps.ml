(* build a map of three bindings, then insert and remove — every step a fresh
   spine, every untouched subtree shared *)
module M = Map.Make (String)

let t = M.of_list [ "beta", 2; "delta", 1; "kappa", 4 ]
let t' = M.add "gamma" 3 t
let t'' = M.remove "beta" t'
let () = print_int (M.cardinal t'')
