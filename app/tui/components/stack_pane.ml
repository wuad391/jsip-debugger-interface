open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Target = struct
  type t =
    | Frame of int
    | Step of int
    | Toggle of int
    | Expand of int
  [@@deriving sexp_of, equal]
end

module Row = struct
  type t =
    { lines : View.t list
    ; call : int
    ; target : Target.t
    ; glyph_x : int option
    ; glyph_target : Target.t option
    ; height : int
    }
end

let has_heat heat = Array.exists heat ~f:Option.is_some

(* call [i]'s descendants are exactly the calls inside its event range — the
   depth bookkeeping Call_stack already did. They PRECEDE it: the wire writes
   an event at call completion, so a subtree ends at its own call. *)
let descendants (calls : Call.t array) index =
  let lo, (_ : int) = calls.(index).Call.range in
  index - lo
;;

let is_hidden ~folds ~calls index =
  Set.exists folds ~f:(fun folded ->
    let lo, (_ : int) = calls.(folded).Call.range in
    lo <= index && index < folded)
;;

(* An exchange-scale dump is thousands of near-identical leaf calls in a row
   — Queue.add while a book fills, Map.find while bots quote. Runs of at
   least [run_threshold] visible leaves repeating one function at one depth
   collapse to a single [fn args ⋯ ×N] row, expandable from its glyph; a run
   holding the selection or a live frame never collapses, so the rows the eye
   is following stay individually visible. *)
let run_threshold = 4

(* entry [i] names the run [i] belongs to, as its [(lo, hi)] span *)
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

(* the collapsed run [index] sits in, if any — where [h] on a member and the
   scroll target need to land *)
let run_head ~calls ~folds ~live ~selected index =
  let spans = run_spans ~calls ~folds ~live ~selected in
  match index >= 0 && index < Array.length spans with
  | false -> None
  | true -> Option.map spans.(index) ~f:fst
;;

(* the whole run, one row per visible call: the live chain bright, everything
   else dimmed; a call with descendants gets a fold glyph, and folding tucks
   its range away behind a [⋯ n] count *)
let rows ~width ~calls ~heat ~live ~selected ~folds ~cursor ~expanded =
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
    | true, _ | false, `Member -> None
    | false, ((`Collapsed_head _ | `Expanded_head | `Plain) as run) ->
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
             Set.mem folds step
             && lo <= selected_step
             && selected_step < step
           | None -> false)
      in
      (* the row the keyboard is aiming at wins the wash: it is the transient
         one, and the selection it is drawn over is where you came from *)
      let is_cursor =
        match cursor with Some cursor -> cursor = step | None -> false
      in
      let bg =
        match is_cursor, is_selected with
        | true, _ -> Some Theme.cursor_bg
        | false, true -> Some Theme.highlight_bg
        | false, false -> None
      in
      let fn_color, fn_bold, args_color =
        match is_cursor, live_index, is_selected with
        | true, _, _ -> Theme.cursor_deep, true, Theme.secondary
        | false, (Some _ | None), true ->
          Theme.highlight_deep, true, Theme.secondary
        | false, Some _, false -> Theme.text, true, Theme.secondary
        | false, None, false -> Theme.ghost, false, Theme.ghost
      in
      (* heat rides on the callee's name itself: its color becomes the
         function's share of sampled compute. The cursor and selection keep
         their own text colors — their washes already say where you are — and
         a call the profile has no data on keeps the state color it would
         have had. *)
      let fn_color =
        match is_cursor || is_selected, heat.(step) with
        | false, Some share -> Theme.heat ~share
        | true, _ | _, None -> fn_color
      in
      let fn_attrs =
        match fn_bold with
        | true -> [ Theme.fg fn_color; Attr.bold ]
        | false -> [ Theme.fg fn_color ]
      in
      let indent = 2 * (call.info.depth - 1) in
      let folded = Set.mem folds step in
      let foldable = descendants calls step > 0 in
      let glyph, glyph_target =
        match run, foldable, folded with
        | `Collapsed_head (_ : int), _, _ -> "▸", Some (Target.Expand step)
        | `Expanded_head, _, _ -> "▾", Some (Target.Expand step)
        | `Plain, false, _ -> " ", None
        | `Plain, true, true -> "▸", Some (Target.Toggle step)
        | `Plain, true, false -> "▾", Some (Target.Toggle step)
      in
      let fn = Function_info.display call.info.function_info in
      let args =
        List.map call.info.arguments ~f:Argument.display
        |> String.concat ~sep:" "
      in
      let hidden_note =
        match run, folded with
        | `Collapsed_head count, _ ->
          (* the whole run behind one row: same shape as a fold's count, but
             ×N — these are repeats, not descendants *)
          [ `Hidden, [%string " ⋯ ×%{count#Int}"] ]
        | (`Expanded_head | `Plain), true ->
          [ `Hidden, [%string " ⋯ %{descendants calls step#Int}"] ]
        | (`Expanded_head | `Plain), false -> []
      in
      (* the row is the call and nothing else: where it was written is
         already on screen, highlighted, in the source pane below — a
         [file.ml:line] chip on every row said it a second time and cost the
         column a third of its width *)
      let available = width - 1 - indent - 2 in
      let wrapped =
        Wrap.spans
          ([ `Fn, fn; `Args, [%string " %{args}"] ] @ hidden_note)
          ~first_width:(max 8 available)
          ~width:(max 8 (available - 2))
      in
      let bar =
        match is_cursor, is_selected with
        | true, _ -> View.text ~attrs:(Theme.fg' Theme.cursor) "▎"
        | false, true -> View.text ~attrs:(Theme.fg' Theme.highlight) "▎"
        | false, false -> View.text " "
      in
      let render_span (tag, text) =
        match tag with
        | `Fn -> View.text ~attrs:fn_attrs text
        | `Args -> View.text ~attrs:(Theme.fg' args_color) text
        | `Hidden ->
          View.text ~attrs:[ Theme.fg Theme.muted; Attr.italic ] text
      in
      let glyph_x = 1 + indent in
      let lines =
        List.mapi wrapped ~f:(fun line_index spans ->
          let lead =
            match line_index with
            | 0 ->
              [ bar
              ; View.text (String.make indent ' ')
              ; View.text ~attrs:(Theme.fg' Theme.secondary) glyph
              ; View.text " "
              ]
            | _ -> [ bar; View.text (String.make (indent + 4) ' ') ]
          in
          Panel.row
            ?bg
            (View.hcat (lead @ List.map spans ~f:render_span))
            ~width)
      in
      let target =
        match live_index with
        | Some frame -> Target.Frame frame
        | None -> Target.Step step
      in
      Some
        { Row.lines
        ; call = step
        ; target
        ; glyph_x =
            (match glyph_target with
             | Some (_ : Target.t) -> Some glyph_x
             | None -> None)
        ; glyph_target
        ; height = List.length lines
        })
;;

(* keep the row the eye is on centered among the wrapped rows: the cursor
   while one is aimed, otherwise the selected call — or the fold hiding it *)
let scroll_offset rows ~height ~calls ~live ~selected ~folds ~cursor =
  let target_step =
    match cursor with
    | Some (_ : int) -> cursor
    | None ->
      (match List.nth live selected with
       | None -> None
       | Some step ->
         (match is_hidden ~folds ~calls step with
          | false -> Some step
          | true ->
            (* the innermost visible fold covering it *)
            Set.filter folds ~f:(fun folded ->
              let lo, (_ : int) = calls.(folded).Call.range in
              lo <= step && step < folded)
            |> Set.min_elt))
  in
  let target_start, target_height, total =
    List.fold
      rows
      ~init:(0, 1, 0)
      ~f:(fun (start, target_height, total) (row : Row.t) ->
        match
          match target_step with
          | Some target -> row.call = target
          | None -> false
        with
        | true -> total, row.height, total + row.height
        | false -> start, target_height, total + row.height)
  in
  Int.min
    (Int.max 0 (target_start + (target_height / 2) - (height / 2)))
    (Int.max 0 (total - height))
;;

let view ~width ~height ~calls ~heat ~live ~selected ~folds ~cursor ~expanded
  =
  let inner_width = Panel.inner_width ~width in
  let inner_height = height - Panel.header_height in
  let rows =
    rows
      ~width:inner_width
      ~calls
      ~heat
      ~live
      ~selected
      ~folds
      ~cursor
      ~expanded
  in
  let offset =
    scroll_offset
      rows
      ~height:inner_height
      ~calls
      ~live
      ~selected
      ~folds
      ~cursor
  in
  let body =
    List.concat_map rows ~f:(fun (row : Row.t) -> row.lines)
    |> fun lines -> List.drop lines offset
  in
  let heat_meta =
    match has_heat heat with false -> "" | true -> " · heat"
  in
  Panel.view
    ~title:"call stack"
    ~meta:
      [%string
        "%{Array.length calls#Int} calls · %{List.length live#Int} \
         live%{heat_meta}"]
    ~width
    ~height
    (View.vcat body)
;;

let target_at
  ~width
  ~height
  ~calls
  ~heat
  ~live
  ~selected
  ~folds
  ~cursor
  ~expanded
  ~x
  ~row
  =
  let inner_width = Panel.inner_width ~width in
  let inner_height = height - Panel.header_height in
  let rows =
    rows
      ~width:inner_width
      ~calls
      ~heat
      ~live
      ~selected
      ~folds
      ~cursor
      ~expanded
  in
  let offset =
    scroll_offset
      rows
      ~height:inner_height
      ~calls
      ~live
      ~selected
      ~folds
      ~cursor
  in
  let target_line = row + offset in
  let rec find rows ~line =
    match rows with
    | [] -> None
    | ({ Row.height; target; glyph_x; glyph_target; call = _; lines = _ } :
        Row.t)
      :: rest ->
      (match line < height with
       | true ->
         (* only the glyph cell on the row's first line toggles *)
         (match glyph_x, glyph_target, line with
          | Some glyph_x, Some glyph_target, 0 when x = glyph_x ->
            Some glyph_target
          | _ -> Some target)
       | false -> find rest ~line:(line - height))
  in
  find rows ~line:target_line
;;

(* the rows [w]/[s] walk: what a fold has not tucked away, minus a collapsed
   run's members — the run is one row, so the cursor lands on its head *)
let visible_calls ~calls ~folds ~live ~selected ~expanded =
  let spans = run_spans ~calls ~folds ~live ~selected in
  List.init (Array.length calls) ~f:Fn.id
  |> List.filter ~f:(fun index ->
    (not (is_hidden ~folds ~calls index))
    &&
    match spans.(index) with
    | Some (lo, (_ : int)) -> index = lo || Set.mem expanded lo
    | None -> true)
;;

let move_cursor ~calls ~live ~selected ~folds ~cursor ~expanded ~direction =
  let visible = visible_calls ~calls ~folds ~live ~selected ~expanded in
  let anchor =
    match cursor with
    | Some (_ : int) -> cursor
    | None -> List.nth live selected
  in
  let index =
    Option.bind anchor ~f:(fun step ->
      List.findi visible ~f:(fun (_ : int) call -> call = step)
      |> Option.map ~f:fst)
  in
  match index with
  (* nothing aimed at, or the anchor is folded away: start at the top *)
  | None -> List.hd visible
  | Some index ->
    List.nth
      visible
      (match direction with `Up -> index - 1 | `Down -> index + 1)
;;

let target_of ~live call =
  match List.findi live ~f:(fun (_ : int) index -> index = call) with
  | Some (frame, (_ : int)) -> Target.Frame frame
  | None -> Target.Step call
;;
