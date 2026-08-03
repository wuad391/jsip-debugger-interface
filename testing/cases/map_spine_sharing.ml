(* Five keys, then one more [add].  A [Map] rebuilds only the path from
   the root down to the new leaf; every subtree hanging off that path is
   the SAME allocation as in the previous version, so the walker emits it
   as an [Id] back-reference instead of walking that subtree again -- and
   the interface draws it once and points at it from the new version.

   [m] is the balanced tree

                       "f"
                     /     \
                  "d"       "h"
                  /            \
               "b"              "j"

   and "g" lands under "h", so [bigger] rebuilds only "f" and "h": the
   whole "d" half and the "j" leaf are shared, and the interface shows
   each of them as an arrow back to the node it already drew. *)
module M = Map.Make (String)

(* the first four versions are built inside a call so they die at the
   collection below, leaving the registry holding just the finished
   five-node tree and the version derived from it *)
let seed () =
  let m1 = M.add "f" 6 M.empty in
  let m2 = M.add "d" 4 m1 in
  let m3 = M.add "h" 8 m2 in
  M.add "b" 2 m3

let () =
  let m = M.add "j" 10 (seed ()) in
  Gc.full_major ();
  let bigger = M.add "g" 7 m in
  ignore bigger
