open! Core

module Binder = struct
  type t = string [@@deriving sexp, compare, equal]

  let display t = t
end

type t = Binder.t String.Map.t [@@deriving sexp]

let empty = String.Map.empty
let binder t ~name = Map.find t name

(* the inner frame's names win, exactly as they do in the source — an [m]
   bound inside a helper is what [m] means while that helper runs *)
let nest ~outer ~inner =
  Map.merge_skewed outer inner ~combine:(fun ~key:_ (_ : Binder.t) inner ->
    inner)
;;
