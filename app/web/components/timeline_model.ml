open! Core

(* enough segments to read a run's phases, few enough that each stays a
   clickable sliver — the mockup draws about a hundred *)
let max_segments = 160

let segments ~density =
  let steps = Array.length density in
  match steps with
  | 0 -> [||]
  | steps ->
    let count = Int.min max_segments steps in
    Array.init count ~f:(fun segment ->
      let lo = segment * steps / count in
      let hi = Int.max lo (((segment + 1) * steps / count) - 1) in
      let rec peak step acc =
        match step > hi with
        | true -> acc
        | false -> peak (step + 1) (Float.max acc density.(step))
      in
      peak lo 0.)
;;

let step_of_fraction ~total ~fraction =
  match total <= 0 with
  | true -> 0
  | false ->
    let step =
      Int.of_float
        (Float.round_nearest (fraction *. Float.of_int (total - 1)))
    in
    Int.max 0 (Int.min (total - 1) step)
;;

let fraction_of_step ~total ~step =
  match total <= 1 with
  | true -> 0.
  | false -> Float.of_int step /. Float.of_int (total - 1)
;;

(* which segment the playhead has reached — segments at or before it draw
   full strength, the future dims *)
let played ~total ~step ~segments:count =
  match total <= 0 || count = 0 with
  | true -> 0
  | false -> (step + 1) * count / total
;;
