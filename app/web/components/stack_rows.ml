open! Core
open Jsip_types

module Target = struct
  type t =
    | Frame of int
    | Step of int
    | Toggle of int
    | Expand of int
  [@@deriving sexp_of, equal]
end

module State = struct
  type t =
    | Selected
    | Live
    | Dimmed
  [@@deriving sexp_of, equal]
end

module Glyph = struct
  type t =
    | Blank
    | Fold of bool (** folded? *)
    | Run of bool (** collapsed? *)
  [@@deriving sexp_of, equal]
end

module Hidden = struct
  type t =
    | Nothing
    | Descendants of int (** the [⋯ n] a fold tucked away *)
    | Repeats of int (** the [⋯ ×N] a collapsed run stands for *)
  [@@deriving sexp_of, equal]
end

module Row = struct
  type t =
    { step : int
    ; depth : int
    ; state : State.t
    ; heat : float option
    ; fn : string
    ; args : string
    ; glyph : Glyph.t
    ; hidden : Hidden.t
    ; registered : string option
    ; target : Target.t
    ; glyph_target : Target.t option
    }
  [@@deriving sexp_of]
end

(* call [i]'s descendants are exactly the calls inside its event range — the
   wire writes an event at call completion, so a subtree ends at its call *)
let descendants (calls : Call.t array) index =
  let lo, (_ : int) = calls.(index).Call.range in
  index - lo
;;

let is_hidden ~folds ~calls index =
  Set.exists folds ~f:(fun folded ->
    let lo, (_ : int) = calls.(folded).Call.range in
    lo <= index && index < folded)
;;

(* Exchange-scale dumps repeat themselves: runs of at least this many
   visible leaves repeating one function at one depth collapse to a single
   [fn args ⋯ ×N] row. A run holding the selection or a live frame never
   collapses, so the rows the eye is following stay individually visible. *)
let run_threshold = 4

let run_spans ~calls ~folds ~live ~selected =
  let length = Array.length calls in
  let spans = Array.create ~len:length None in
  let selected_step = List.nth live selected in
  let protected_ lo hi =
    List.exists live ~f:(fun step -> lo <= step && step <= hi)
    || Option.value_map selected_step ~default:false ~f:(fun step ->
      lo <= step && step <= hi)
  in
  let collapsible index =
    (not (is_hidden ~folds ~calls index)) && descendants calls index = 0
  in
  let key index =
    let call = calls.(index) in
    Function_info.display call.Call.info.function_info, call.info.depth
  in
  let rec walk lo =
    match lo < length with
    | false -> ()
    | true ->
      (match collapsible lo with
       | false -> walk (lo + 1)
       | true ->
         let rec stop i =
           match
             i < length
             && collapsible i
             && [%equal: string * int] (key i) (key lo)
           with
           | true -> stop (i + 1)
           | false -> i
         in
         let hi = stop (lo + 1) - 1 in
         (match hi - lo + 1 >= run_threshold && not (protected_ lo hi) with
          | true ->
            for i = lo to hi do
              spans.(i) <- Some (lo, hi)
            done
          | false -> ());
         walk (hi + 1))
  in
  walk 0;
  spans
;;

let run_head ~calls ~folds ~live ~selected index =
  let spans = run_spans ~calls ~folds ~live ~selected in
  match index >= 0 && index < Array.length spans with
  | false -> None
  | true -> Option.map spans.(index) ~f:fst
;;

let target_of ~live call =
  match List.findi live ~f:(fun (_ : int) index -> index = call) with
  | Some (frame, (_ : int)) -> Target.Frame frame
  | None -> Target.Step call
;;

let rows ~calls ~heat ~live ~selected ~folds ~expanded ~registered =
  let spans = run_spans ~calls ~folds ~live ~selected in
  Array.to_list calls
  |> List.filter_mapi ~f:(fun step (call : Call.t) ->
    let run =
      match spans.(step) with
      | Some (lo, hi) when not (Set.mem expanded lo) ->
        (match step = lo with
         | true -> `Collapsed_head (hi - lo + 1)
         | false -> `Member)
      | Some (lo, (_ : int)) when step = lo -> `Expanded_head
      | Some (_ : int * int) | None -> `Plain
    in
    match is_hidden ~folds ~calls step, run with
    | true, (`Collapsed_head (_ : int) | `Expanded_head | `Member | `Plain)
    | false, `Member ->
      None
    | false, ((`Collapsed_head _ | `Expanded_head | `Plain) as run) ->
      Some (step, call, run))
  |> List.map ~f:(fun (step, (call : Call.t), run) ->
    let live_index =
      List.findi live ~f:(fun (_ : int) index -> index = step)
      |> Option.map ~f:fst
    in
    let is_selected =
      match live_index with
      | Some frame -> frame = selected
      | None ->
        (* a fold hiding the selected call lights up in its place *)
        (match List.nth live selected with
         | Some selected_step ->
           let lo, (_ : int) = call.range in
           Set.mem folds step && lo <= selected_step && selected_step < step
         | None -> false)
    in
    let state =
      match is_selected, live_index with
      | true, (Some _ | None) -> State.Selected
      | false, Some (_ : int) -> State.Live
      | false, None -> State.Dimmed
    in
    let folded = Set.mem folds step in
    let foldable = descendants calls step > 0 in
    let glyph, glyph_target =
      match run, foldable, folded with
      | `Collapsed_head (_ : int), (_ : bool), (_ : bool) ->
        Glyph.Run true, Some (Target.Expand step)
      | `Expanded_head, (_ : bool), (_ : bool) ->
        Glyph.Run false, Some (Target.Expand step)
      | `Plain, false, (_ : bool) -> Glyph.Blank, None
      | `Plain, true, folded -> Glyph.Fold folded, Some (Target.Toggle step)
    in
    let hidden =
      match run, folded with
      | `Collapsed_head count, (_ : bool) -> Hidden.Repeats count
      | (`Expanded_head | `Plain), true ->
        Hidden.Descendants (descendants calls step)
      | (`Expanded_head | `Plain), false -> Hidden.Nothing
    in
    { Row.step
    ; depth = call.info.depth
    ; state
    ; heat = heat.(step)
    ; fn = Function_info.display call.info.function_info
    ; args =
        List.map call.info.arguments ~f:Argument.display
        |> String.concat ~sep:" "
    ; glyph
    ; hidden
    ; registered = registered.(step)
    ; target =
        (match live_index with
         | Some frame -> Target.Frame frame
         | None -> Target.Step step)
    ; glyph_target
    })
;;
