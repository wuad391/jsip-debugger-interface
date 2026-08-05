open! Core
open Bonsai.Let_syntax
open Jsip_types
open Jsip_replay
open Jsip_tui_components
module Attr = Bonsai_term.Attr
module Effect = Bonsai_term.Effect
module Event = Bonsai_term.Event
module Dimensions = Bonsai_term.Dimensions
module Position = Bonsai_term.Position
module Region = Bonsai_term.Region
module View = Bonsai_term.View

let play_interval = Time_ns.Span.of_int_ms 850

(* a source fold is per file: the dump may span several *)
module Source_fold = struct
  module T = struct
    type t =
      { file : string
      ; line : int
      }
    [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

(* which pane the keyboard drives; [Tab] cycles. The source pane has nothing
   to select, so it is not in the rotation, and a shut flame drawer is
   skipped for the same reason — its title row has nothing to aim at. *)
module Pane = struct
  type t =
    | Stack
    | Heap
    | Flame
  [@@deriving sexp_of, equal]

  let next t ~flame_open =
    match t, flame_open with
    | Stack, (true | false) -> Heap
    | Heap, true -> Flame
    | Heap, false -> Stack
    | Flame, (true | false) -> Stack
  ;;
end

(* the flame drawer's own state. [open_] is the drawer; the rest outlives it,
   because a flame path addresses the static call tree — shutting the drawer
   and opening it again should not lose where you were. *)
module Flame = struct
  type t =
    { open_ : bool
    ; zoom : Flame_pane.Path.t
    (** the bar the drawer is scaled to; [[]] is the whole tree *)
    ; cursor : Flame_pane.Path.t option
    ; depth_scroll : int
    }
  [@@deriving sexp_of, equal]

  let initial = { open_ = false; zoom = []; cursor = None; depth_scroll = 0 }
end

module Model = struct
  type t =
    { step : int
    ; selected_frame : int option (** [None] = innermost frame *)
    ; playing : bool
    ; heap_scroll : int (** in canvas lines: heap rows wrap *)
    ; heap_folds : Set.M(Heap_pane.Fold).t
    ; stack_folds : Int.Set.t
    ; stack_expanded : Int.Set.t
    (** repeat runs (keyed by head index) opened back up out of their
        collapsed [⋯ ×N] row *)
    ; source_folds : Set.M(Source_fold).t
    ; focus : Pane.t
    ; heap_selected : Heap_pane.Spot.t option
    (** [None] falls back to the structure this step walked, which is what
        the pane highlighted before selection existed *)
    ; heap_cursor : Heap_pane.Spot.t option
    ; stack_cursor : int option (** a call index *)
    ; stack_scroll : int
    (** the wheel's offset over the stack's own centering; any step or aim
        resets it, so the hand wins between moves and the centering wins
        whenever something moves *)
    ; accordion : bool
    (** [z]: every structure collapsed but the one the keyboard is in *)
    ; heap_filter : string
    (** [/]: only structures whose header matches stay on the canvas. Empty =
        no filter. *)
    ; typing_filter : bool
    (** the [/] prompt is open, and keystrokes edit the filter instead of
        driving the panes *)
    ; stack_collapsed : bool
    (** the call stack folded to its title row, its height handed to the
        source pane — [1], or a click on the title *)
    ; source_collapsed : bool (** likewise the source pane, on [2] *)
    ; sort_by_address : bool
    (** [o]: the outline's top level in ascending address order, so memory
        locality reads as adjacency. Off — the default — is registry order,
        i.e. creation order. *)
    ; popped_out : Heap_pane.Spot.t option
    (** [Enter] in the heap: the row whose structure is drawn as a diagram
        over the panes. While it is up it owns the keyboard — so the step
        cannot move under it — and [Escape] backs out. *)
    ; pop_scroll : int
    ; pop_pan : int
    ; flame : Flame.t
    (** [f]: the flame drawer under the heap, open or shut *)
    }
  [@@deriving sexp_of, equal]

  let initial =
    { step = 0
    ; selected_frame = None
    ; playing = false
    ; heap_scroll = 0
    ; heap_folds = Set.empty (module Heap_pane.Fold)
    ; stack_folds = Int.Set.empty
    ; stack_expanded = Int.Set.empty
    ; source_folds = Set.empty (module Source_fold)
    ; focus = Pane.Heap
    ; heap_selected = None
    ; heap_cursor = None
    ; stack_cursor = None
    ; stack_scroll = 0
    ; accordion = false
    ; heap_filter = ""
    ; typing_filter = false
    ; stack_collapsed = false
    ; source_collapsed = false
    ; sort_by_address = false
    ; popped_out = None
    ; pop_scroll = 0
    ; pop_pan = 0
    ; flame = Flame.initial
    }
  ;;
end

module Action = struct
  type t =
    | Step_to of int
    | Step_delta of int
    | Tick
    | Toggle_play
    | Select_frame of int
    | Scroll_heap of int
    | Scroll_stack of int
    | Toggle_heap_fold of Heap_pane.Fold.t
    | Toggle_stack_fold of int
    | Toggle_stack_run of int
    (** open/close the repeat run headed at this call *)
    | Toggle_source_fold of Source_fold.t
    | Focus_next_pane
    | Move_cursor of Heap_pane.Direction.t
    (** [wasd]: aim, without committing *)
    | Commit_cursor (** [Enter]: the orange becomes the blue *)
    | Jump_cursor of Heap_pane.Direction.t
    (** shift + [wasd]: aim and commit in one, skipping the orange *)
    | Select_heap_node of Heap_pane.Spot.t (** a click on a row *)
    | Toggle_focused_fold
    (** [space]: collapse whatever the focused pane is pointing at — the
        structure a heap row belongs to, or a call's descendants *)
    | Toggle_accordion
    | Toggle_stack_pane
    | Toggle_source_pane
    | Toggle_address_order
    (** [o]: flip the outline's top level between registry (creation) order
        and ascending address order *)
    | Focus_latest
    (** [.]: back to the latest change — the structure this step walked —
        clearing the aim, the chosen row, and the scroll on the way *)
    | Begin_filter (** [/]: open the prompt, starting from empty *)
    | Filter_input of char
    | Filter_backspace
    | Commit_filter (** [Enter]: close the prompt, keep the filter *)
    | Cancel_filter (** [Escape]: close the prompt, drop the filter *)
    | Pop_out
    (** [Enter] on a heap row: its structure as the diagram it physically is,
        over the panes. Stops playback, because the slab would otherwise be
        showing a step that has moved on. *)
    | Dismiss_pop_out
    (** [Escape]: back to the outline, nothing else moved *)
    | Commit_pop_out
    (** [Enter] again: take the jump the row itself would have taken — to the
        step that allocated it — and close. Escape backs out, Enter goes
        through, which is what those two keys mean everywhere. *)
    | Scroll_pop_out of int
    | Pan_pop_out of int
    | Toggle_flame (** [f]: the flame drawer in, and back out *)
    | Jump_flame of Flame_pane.Path.t (** a click on a bar *)
    | Zoom_flame (** [z] while the drawer is focused: rescale to the bar *)
    | Reset_flame_zoom (** [Z]: back to the whole tree *)
    | Scroll_flame of int (** by depth rows *)
  [@@deriving sexp_of]
end

let frame_count replay ~step =
  List.length (Replay.step_exn replay ~step).frames
;;

(* Indices into a list that can be empty, so [max] is [-1] there and
   [Int.clamp_exn] would assert rather than clamp. *)
let clamp index ~max = Int.max 0 (Int.min max index)

(* the blue row: whatever was last chosen, or — until something is — the root
   of the structure this step walked, which is what the pane highlighted
   before selection existed. Both the renderer and the cursor arithmetic go
   through here so they cannot disagree about where the cursor starts. *)
let heap_selection replay (model : Model.t) =
  let selected =
    match model.heap_selected with
    | Some (_ : Heap_pane.Spot.t) -> model.heap_selected
    | None ->
      List.find
        (Replay.step_exn replay ~step:model.step).structures
        ~f:(fun (structure : Replay.Structure.t) -> structure.is_current)
      |> Option.map ~f:Heap_pane.spot_of_structure
  in
  { Heap_pane.Selection.selected; cursor = model.heap_cursor }
;;

(* What the heap pane actually draws: the [/] filter thins the registry to
   the structures whose header matches, [o] re-sorts the survivors by
   address, and accordion mode closes every structure but the one the
   keyboard is in. The renderer, the hit-tests, the cursor arithmetic and the
   landing all go through here so they cannot disagree about what is on the
   canvas. *)
let heap_inputs replay (model : Model.t) =
  let structures =
    List.filter
      (Replay.step_exn replay ~step:model.step).structures
      ~f:(fun structure ->
        Heap_pane.matches_filter structure ~filter:model.heap_filter)
  in
  let structures =
    match model.sort_by_address with
    | false -> structures
    | true -> Heap_pane.by_address structures
  in
  let folds =
    match model.accordion with
    | false -> model.heap_folds
    | true ->
      Heap_pane.accordion_folds
        ~structures
        ~folds:model.heap_folds
        ~selection:(heap_selection replay model)
  in
  structures, folds
;;

(* Where a step (or a committed jump) lands the heap pane: the selection —
   the walked structure's root until something is chosen — brought into view.
   Needs the terminal's dimensions, which reach [apply_action] as the state
   machine's input; while the app is inactive there is nothing to aim, so the
   top of the canvas does fine. *)
let landing replay dimensions (model : Model.t) =
  match (dimensions : Dimensions.t Bonsai.Computation_status.t) with
  | Inactive -> 0
  | Active dimensions ->
    let layout =
      Layout.compute
        ~stack_collapsed:model.stack_collapsed
        ~source_collapsed:model.source_collapsed
        dimensions
        ~flame_open:model.flame.open_
    in
    let { Replay.Step.nodes; new_addresses; _ } =
      Replay.step_exn replay ~step:model.step
    in
    let structures, folds = heap_inputs replay model in
    Heap_pane.landing
      ~structures
      ~nodes
      ~new_addresses
      ~folds
      ~selection:(heap_selection replay model)
      ~width:layout.heap.width
      ~height:layout.heap.height
;;

(* the frame the stack pane treats as selected — the renderer's [selected],
   recomputed here because cursor movement starts from it *)
let selected_frame replay (model : Model.t) =
  let count = frame_count replay ~step:model.step in
  clamp
    (Option.value model.selected_frame ~default:(count - 1))
    ~max:(count - 1)
;;

let live_calls replay ~step =
  List.map (Replay.step_exn replay ~step).frames ~f:(fun (frame : Call.t) ->
    snd frame.range)
;;

let apply_action
  replay
  ~calls
  ~births
  ~flame
  (_ : _ Bonsai.Apply_action_context.t)
  (dimensions : Dimensions.t Bonsai.Computation_status.t)
  (model : Model.t)
  action
  =
  (* the flame panel apportions its bars against the screen, so moving its
     cursor needs the width the picture was drawn at. An inactive machine
     cannot be driven, so its zero simply makes every move a no-op. *)
  let flame_width =
    match dimensions with
    | Active { width; height = (_ : int) } -> width
    | Inactive -> 0
  in
  let last = Replay.length replay - 1 in
  let clamp_step step = clamp step ~max:last in
  let clamp_frame index =
    clamp index ~max:(frame_count replay ~step:model.step - 1)
  in
  (* every fold set is a "click it again to undo it" toggle *)
  let toggle set x =
    match Set.mem set x with
    | true -> Set.remove set x
    | false -> Set.add set x
  in
  (* any move re-follows the innermost frame and drops both cursors and the
     chosen row — at another step those addresses name nothing, so blue goes
     back to following the walked structure. Folds persist; that is the point
     of keying them stably. So does the flame drawer: its paths address the
     static call tree rather than a step's addresses, and watching the lit
     path move as you step is the whole reason the drawer stays open. *)
  let move ~playing step =
    let stepped =
      { model with
        step = clamp_step step
      ; selected_frame = None
      ; playing
      ; heap_scroll = 0
      ; heap_selected = None
      ; heap_cursor = None
      ; stack_cursor = None
      ; stack_scroll = 0
      }
    in
    (* land the eye on what the step walked, not on the canvas top *)
    { stepped with heap_scroll = landing replay dimensions stepped }
  in
  (* committing a heap row is exactly what clicking it does — jump to where
     it was allocated — and additionally pins it as the selection, so the row
     stays blue and keeps showing its address at the new step *)
  let select_heap_node model (spot : Heap_pane.Spot.t) =
    let stepped =
      match Map.find births spot.address with
      | Some step -> move ~playing:false step
      | None -> model
    in
    (* the jump lands on a different canvas, where this node may be drawn
       somewhere else entirely — follow it there rather than losing it. It
       resolves against the manual folds, not the accordion's: the accordion
       reshapes itself around whatever this lands on, so the structure it
       opens is exactly the one the resolved spot lives in. *)
    let { Replay.Step.nodes; new_addresses; _ } =
      Replay.step_exn replay ~step:stepped.step
    in
    let structures, (_ : Set.M(Heap_pane.Fold).t) =
      heap_inputs replay stepped
    in
    let selected =
      Heap_pane.resolve_spot
        ~structures
        ~nodes
        ~new_addresses
        ~folds:stepped.heap_folds
        spot
    in
    (* the landing scroll [move] computed aimed at the walked structure; now
       that the selection is the committed row, land on that instead *)
    let landed =
      { stepped with heap_selected = selected; heap_cursor = None }
    in
    { landed with heap_scroll = landing replay dimensions landed }
  in
  let flame_jump (model : Model.t) path =
    match Flame_tree.find flame ~path with
    | None -> model
    | Some (node : Flame_tree.Node.t) ->
      let stepped = move ~playing:false node.first_step in
      { stepped with flame = { model.flame with cursor = Some path } }
  in
  let flame_aim (model : Model.t) ~direction =
    let moved =
      Flame_pane.move_cursor
        ~width:flame_width
        ~tree:flame
        ~zoom:model.flame.zoom
        ~cursor:model.flame.cursor
        ~live:
          (Flame_pane.live_path
             flame
             ~frames:(Replay.step_exn replay ~step:model.step).frames)
        ~direction
    in
    match moved with
    | None -> model
    | Some path ->
      { model with flame = { model.flame with cursor = Some path } }
  in
  let commit (model : Model.t) =
    match model.focus with
    | Pane.Heap ->
      (match model.heap_cursor with
       | None -> model
       | Some spot -> select_heap_node model spot)
    | Pane.Stack ->
      (match model.stack_cursor with
       | None -> model
       | Some call ->
         (match
            Stack_pane.target_of
              ~live:(live_calls replay ~step:model.step)
              call
          with
          | Stack_pane.Target.Frame index ->
            { model with
              selected_frame = Some (clamp_frame index)
            ; stack_cursor = None
            }
          | Stack_pane.Target.Step step -> move ~playing:false step
          | Stack_pane.Target.Toggle (_ : int)
          | Stack_pane.Target.Expand (_ : int) ->
            model))
    | Pane.Flame ->
      (* a bar stands for every call that merged into it; committing goes to
         the first of them, which is what clicking it does *)
      (match model.flame.cursor with
       | None -> model
       | Some path -> flame_jump model path)
  in
  let aim (model : Model.t) ~direction =
    match model.focus with
    | Pane.Heap ->
      let { Replay.Step.nodes; new_addresses; _ } =
        Replay.step_exn replay ~step:model.step
      in
      let structures, folds = heap_inputs replay model in
      let moved =
        Heap_pane.move_cursor
          ~structures
          ~nodes
          ~new_addresses
          ~folds
          ~selection:(heap_selection replay model)
          ~direction
      in
      (match moved with
       | None -> model
       | Some spot -> { model with heap_cursor = Some spot })
    | Pane.Stack ->
      let vertical =
        match (direction : Heap_pane.Direction.t) with
        | Up -> Some `Up
        | Down -> Some `Down
        | Left | Right -> None
      in
      (match vertical with
       | None -> model
       | Some direction ->
         let moved =
           Stack_pane.move_cursor
             ~calls
             ~live:(live_calls replay ~step:model.step)
             ~selected:(selected_frame replay model)
             ~folds:model.stack_folds
             ~cursor:model.stack_cursor
             ~expanded:model.stack_expanded
             ~direction
         in
         (match moved with
          | None -> model
          (* aiming re-centers on the cursor; the wheel's offset yields *)
          | Some call ->
            { model with stack_cursor = Some call; stack_scroll = 0 }))
    | Pane.Flame ->
      flame_aim
        model
        ~direction:
          (match (direction : Heap_pane.Direction.t) with
           | Up -> Flame_pane.Direction.Up
           | Down -> Flame_pane.Direction.Down
           | Left -> Flame_pane.Direction.Left
           | Right -> Flame_pane.Direction.Right)
  in
  (* while the accordion is driving, structure folds are its to decide — a
     manual toggle would vanish under the override now and pop back as a
     surprise when the mode goes off. Node folds keep working. *)
  let toggle_heap_fold (model : Model.t) fold =
    match model.accordion, (fold : Heap_pane.Fold.t) with
    | true, Structure (_ : int) -> model
    | false, Structure (_ : int)
    | (true | false), Node ((_ : int), (_ : int list)) ->
      { model with heap_folds = toggle model.heap_folds fold }
  in
  (* a bar names a whole call path, so what a click or [Enter] jumps to is
     the first of the calls that merged into it; the cursor pins there so the
     panel still says where you came from at the new step *)
  match (action : Action.t) with
  | Step_to step -> move ~playing:false step
  | Step_delta delta ->
    let step = clamp_step (model.step + delta) in
    move ~playing:(model.playing && delta > 0 && step < last) step
  | Tick ->
    (match model.playing with
     | false -> model
     | true ->
       let step = clamp_step (model.step + 1) in
       move ~playing:(step < last) step)
  | Toggle_play -> { model with playing = not model.playing }
  | Select_frame index ->
    { model with
      selected_frame = Some (clamp_frame index)
    ; stack_scroll = 0
    }
  | Scroll_heap delta ->
    { model with heap_scroll = Int.max 0 (model.heap_scroll + delta) }
  (* unclamped here — the pane clamps against the rows it actually drew, and
     it is the one place that knows their heights *)
  | Scroll_stack delta ->
    { model with stack_scroll = model.stack_scroll + delta }
  | Toggle_heap_fold fold -> toggle_heap_fold model fold
  | Toggle_stack_fold call ->
    { model with stack_folds = toggle model.stack_folds call }
  | Toggle_stack_run head ->
    { model with stack_expanded = toggle model.stack_expanded head }
  | Toggle_source_fold fold ->
    { model with source_folds = toggle model.source_folds fold }
  (* [h] folds what you are pointing at: in the heap, the node under the
     cursor (its children tuck behind the row) or — on a structure's own row
     — the whole structure; in the stack the aimed call's range. It reads the
     cursor first and the selection second, so it works before you have aimed
     at anything — the heap falls back to the structure this step walked, the
     stack to the selected frame. *)
  | Toggle_focused_fold ->
    (match model.focus with
     | Pane.Heap ->
       let { Heap_pane.Selection.selected; cursor } =
         heap_selection replay model
       in
       (match Option.first_some cursor selected with
        | None -> model
        | Some spot ->
          (* the spot can name a row the outline is not drawing — stepping
             drops the cursor, and the fallback is the walked structure's
             root row, which a collapsed structure hides. Fold what is
             actually on screen: collapsed, the only drawing of that node is
             the structure's own row, so [h] reopens the structure instead of
             flipping a node fold nobody can see. *)
          let { Replay.Step.nodes; new_addresses; _ } =
            Replay.step_exn replay ~step:model.step
          in
          let structures, folds = heap_inputs replay model in
          (match
             Heap_pane.resolve_spot
               ~structures
               ~nodes
               ~new_addresses
               ~folds
               spot
           with
           | None -> model
           | Some spot ->
             toggle_heap_fold model (Heap_pane.fold_of_spot spot)))
     | Pane.Stack ->
       let live = live_calls replay ~step:model.step in
       (match
          Option.first_some
            model.stack_cursor
            (List.nth live (selected_frame replay model))
        with
        | None -> model
        | Some call ->
          (* a call inside a repeat run has no descendants to fold — for it,
             [h] opens and closes the run instead *)
          (match
             Stack_pane.run_head
               ~calls
               ~folds:model.stack_folds
               ~live
               ~selected:(selected_frame replay model)
               call
           with
           | Some head ->
             { model with stack_expanded = toggle model.stack_expanded head }
           | None ->
             { model with stack_folds = toggle model.stack_folds call }))
     (* nothing to fold in a flame graph: a bar's children ARE the row under
        it, so hiding them would leave a hole where its callees ran. [z]
        zooms instead, which is the useful move here. *)
     | Pane.Flame -> model)
  | Toggle_accordion -> { model with accordion = not model.accordion }
  | Toggle_stack_pane ->
    let stack_collapsed = not model.stack_collapsed in
    (* a collapsed pane cannot be driven; hand the keyboard to the heap *)
    { model with
      stack_collapsed
    ; focus =
        (match stack_collapsed, model.focus with
         | true, Pane.Stack -> Pane.Heap
         | (_ : bool), focus -> focus)
    }
  | Toggle_source_pane ->
    { model with source_collapsed = not model.source_collapsed }
  | Toggle_address_order ->
    { model with sort_by_address = not model.sort_by_address }
  (* re-landing on the current step is exactly what stepping to it does *)
  | Focus_latest -> move ~playing:model.playing model.step
  (* [/] always starts from empty: the old filter was shaped around whatever
     you were hunting last time, and editing it beats out of a prompt this
     small costs more keys than retyping *)
  | Begin_filter -> { model with typing_filter = true; heap_filter = "" }
  | Filter_input char ->
    { model with heap_filter = model.heap_filter ^ String.of_char char }
  (* backspacing past the last character backs out of the prompt itself — the
     natural exit when your hands are already on that key. [Escape] still
     quits from anywhere, but a prompt you can only leave with a key you were
     not using is a trap. *)
  | Filter_backspace ->
    (match String.is_empty model.heap_filter with
     | true -> { model with typing_filter = false }
     | false ->
       { model with heap_filter = String.drop_suffix model.heap_filter 1 })
  | Commit_filter -> { model with typing_filter = false }
  | Cancel_filter -> { model with typing_filter = false; heap_filter = "" }
  | Focus_next_pane ->
    { model with
      focus = Pane.next model.focus ~flame_open:model.flame.open_
    }
  | Move_cursor direction -> aim model ~direction
  | Commit_cursor -> commit model
  | Jump_cursor direction -> commit (aim model ~direction)
  | Select_heap_node spot -> select_heap_node model spot
  (* the diagram of the row you are on — the cursor first and the selection
     second, the same reading [h] takes, so it works before you have aimed at
     anything. Playback stops: the slab is a look at one step. *)
  | Pop_out ->
    let { Heap_pane.Selection.selected; cursor } =
      heap_selection replay model
    in
    (match Option.first_some cursor selected with
     | None -> model
     | Some spot ->
       { model with
         popped_out = Some spot
       ; pop_scroll = 0
       ; pop_pan = 0
       ; playing = false
       })
  | Dismiss_pop_out -> { model with popped_out = None }
  | Commit_pop_out ->
    (match model.popped_out with
     | None -> model
     | Some spot -> select_heap_node { model with popped_out = None } spot)
  | Scroll_pop_out delta ->
    { model with pop_scroll = Int.max 0 (model.pop_scroll + delta) }
  | Pan_pop_out delta ->
    { model with pop_pan = Int.max 0 (model.pop_pan + delta) }
  | Toggle_flame ->
    let open_ = not model.flame.open_ in
    { model with
      flame = { model.flame with open_ }
    ; (* a shut drawer leaves [Tab]'s rotation, so the keyboard must not be
         left standing in one *)
      focus =
        (match open_, model.focus with
         | false, Pane.Flame -> Pane.Heap
         | false, (Pane.Stack | Pane.Heap)
         | true, (Pane.Stack | Pane.Heap | Pane.Flame) ->
           model.focus)
    }
  | Jump_flame path -> flame_jump model path
  | Zoom_flame ->
    (match model.flame.cursor with
     | None -> model
     | Some cursor ->
       { model with
         flame = { model.flame with zoom = cursor; depth_scroll = 0 }
       })
  | Reset_flame_zoom ->
    { model with flame = { model.flame with zoom = []; depth_scroll = 0 } }
  | Scroll_flame delta ->
    { model with
      flame =
        { model.flame with
          depth_scroll = Int.max 0 (model.flame.depth_scroll + delta)
        }
    }
;;

(* each call's share, joined once up front — the color its name renders in.
   With a perf profile the share is sampled compute; without one it falls
   back to the trace itself — each function's share of the dump's events — so
   a replay with no [-perf-file] (a project-mode capture, say) still reads
   hot-to-cold at a glance. The session bar's legend names which of the two
   the colors mean. *)
let heat_of_calls ~profile ~(calls : Call.t array) =
  match (profile : Heat_profile.t option) with
  | Some profile ->
    ( Array.map calls ~f:(fun (call : Call.t) ->
        Heat_profile.share
          profile
          ~function_info:call.info.function_info
          ~location:call.info.location)
    , `Compute )
  | None ->
    let counts =
      Array.fold calls ~init:String.Map.empty ~f:(fun counts call ->
        Map.update
          counts
          (Function_info.display call.info.function_info)
          ~f:(fun count -> 1 + Option.value count ~default:0))
    in
    let total = Float.of_int (Array.length calls) in
    ( Array.map calls ~f:(fun (call : Call.t) ->
        Map.find counts (Function_info.display call.info.function_info)
        |> Option.map ~f:(fun count -> Float.of_int count /. total))
    , `Calls )
;;

(* how much allocated at each step — the timeline's density shading — as each
   step's share of the run's busiest step *)
let density_of_steps replay =
  let counts =
    Array.init (Replay.length replay) ~f:(fun step ->
      Set.length (Replay.step_exn replay ~step).new_addresses)
  in
  let busiest = Array.fold counts ~init:1 ~f:Int.max in
  Array.map counts ~f:(fun count ->
    Float.of_int count /. Float.of_int busiest)
;;

(* What each step's call put into the registry, by the name the heap pane
   lists it under — the [· m] / [· #826] tag on that call's row. A step
   registers whatever its registry carries that the previous step's did not;
   step 0 owns everything registered before the first event, which on a real
   program is the module-init flood, so several names compress to a
   first…last range. *)
let registrations replay =
  let ids ~step =
    List.map
      (Replay.step_exn replay ~step).structures
      ~f:(fun (structure : Replay.Structure.t) -> structure.id)
    |> Int.Set.of_list
  in
  Array.init (Replay.length replay) ~f:(fun step ->
    let before =
      match step with 0 -> Int.Set.empty | step -> ids ~step:(step - 1)
    in
    let fresh =
      List.filter
        (Replay.step_exn replay ~step).structures
        ~f:(fun (structure : Replay.Structure.t) ->
          not (Set.mem before structure.id))
    in
    match fresh with
    | [] -> None
    | [ structure ] -> Some (Replay.Structure.display structure)
    | first :: (_ :: _ as rest) ->
      let last = List.last_exn rest in
      Some
        [%string
          "%{Replay.Structure.display first}…%{Replay.Structure.display \
           last}"])
;;

(* where each address was first seen — what a click on a heap node jumps to *)
let birth_steps replay =
  List.fold
    (List.init (Replay.length replay) ~f:Fn.id)
    ~init:Snapshot.Address.Map.empty
    ~f:(fun births step ->
      Set.fold
        (Replay.step_exn replay ~step).new_addresses
        ~init:births
        ~f:(fun births address -> Map.add_exn births ~key:address ~data:step))
;;

module Computed = struct
  type t =
    { view : View.t
    ; on_click : Position.t -> [ `Act of Action.t | `Quit ] option
    ; on_scroll :
        Position.t -> [ `Up | `Down ] -> sideways:bool -> Action.t option
    }
end

let render
  ~replay
  ~sources
  ~dump_name
  ~calls
  ~heat
  ~heat_source
  ~density
  ~registered
  ~flame
  ~(model : Model.t)
  ~dimensions
  =
  let panel = model.flame in
  let layout =
    Layout.compute
      ~stack_collapsed:model.stack_collapsed
      ~source_collapsed:model.source_collapsed
      dimensions
      ~flame_open:panel.open_
  in
  (* the chip row names the keys that actually work, and focus decides what
     [z] is, so it needs both bits *)
  let flame_chip : Transport.Flame_state.t =
    match panel.open_, model.focus with
    | false, (Pane.Stack | Pane.Heap | Pane.Flame) -> Shut
    | true, Pane.Flame -> Focused
    | true, (Pane.Stack | Pane.Heap) -> Open
  in
  let { Replay.Step.call; frames; structures; nodes; new_addresses } =
    Replay.step_exn replay ~step:model.step
  in
  (* a frame's own event index closes its range — its children came first *)
  let live = List.map frames ~f:(fun (frame : Call.t) -> snd frame.range) in
  (* the same stack, as the merged bars it runs through: the path the panel
     lights up, and the one it starts the keyboard from *)
  let flame_live = Flame_pane.live_path flame ~frames in
  let selected = selected_frame replay model in
  let frame = Option.value (List.nth frames selected) ~default:call in
  let location = frame.info.location in
  let callsite_line =
    match List.nth frames (selected - 1) with
    | Some parent when selected > 0 ->
      let parent_location = parent.info.location in
      (match
         String.equal
           (Location.file_path parent_location)
           (Location.file_path location)
       with
       | true -> Some (Location.line_number parent_location)
       | false -> None)
    | Some _ | None -> None
  in
  let source =
    match Map.find sources (Location.file_path location) with
    | Some loaded -> loaded
    | None ->
      Or_error.error_s
        [%message
          "no source loaded for" ~file:(Location.file_path location : string)]
  in
  let file_path = Location.file_path location in
  let source_folds =
    Set.fold
      model.source_folds
      ~init:Int.Set.empty
      ~f:(fun acc { Source_fold.file; line } ->
        match String.equal file file_path with
        | true -> Set.add acc line
        | false -> acc)
  in
  let snapshot = call.info.snapshot in
  let selection = heap_selection replay model in
  let heap_structures, heap_folds = heap_inputs replay model in
  (* the meta line owns up to the modes shaping the canvas: the filter as
     typed (a block cursor while the prompt is open), the accordion and the
     address order by name *)
  let heap_note =
    let filter =
      match model.typing_filter with
      | true -> Some [%string "/%{model.heap_filter}▌"]
      | false ->
        (match String.is_empty model.heap_filter with
         | true -> None
         | false -> Some [%string "/%{model.heap_filter}"])
    in
    let accordion =
      match model.accordion with true -> Some "accordion" | false -> None
    in
    let order =
      match model.sort_by_address with
      | true -> Some "by address"
      | false -> None
    in
    match List.filter_opt [ filter; accordion; order ] with
    | [] -> None
    | parts -> Some (String.concat parts ~sep:" · ")
  in
  let place (region : Region.t) view =
    View.pad ~l:region.x ~t:region.y view
  in
  (* the focused pane's four seams, redrawn heavy and orange over the light
     gray ones. Panes have no borders of their own, so focus has to be said
     with the lines that already bound them; the two ends of the horizontal
     runs get the junction glyph for whichever light rule they cut across. *)
  let focus_outline (region : Region.t) ~top ~bottom ~joints =
    let color = Theme.cursor in
    (* A seam exists only where a divider does — the screen's own edges are
       not lines — so a pane flush against one is outlined on three sides.
       [joints] names every cell where some other rule runs into the outline;
       they are drawn last, over the runs that cross them, because the tee
       they need is not the glyph either run would have put there. *)
    let left = region.x - 1 in
    let right = region.x + region.width in
    let start = max 0 left in
    let stop = min dimensions.Dimensions.width (right + 1) in
    let on_screen x = x >= 0 && x < dimensions.Dimensions.width in
    let vertical x =
      match on_screen x with
      | false -> []
      | true ->
        [ View.pad
            ~l:x
            ~t:region.y
            (Panel.vertical_rule ~height:region.height ~color)
        ]
    in
    [ View.pad
        ~l:start
        ~t:top
        (Panel.horizontal_rule ~width:(stop - start) ~color)
    ; View.pad
        ~l:start
        ~t:bottom
        (Panel.horizontal_rule ~width:(stop - start) ~color)
    ]
    @ vertical left
    @ vertical right
    |> List.rev_append
         (List.filter_map joints ~f:(fun (x, y, glyph) ->
            match on_screen x with
            | false -> None
            | true -> Some (View.pad ~l:x ~t:y (Panel.junction ~color glyph))))
  in
  let focus_views =
    let seam = layout.column_divider.x in
    match model.focus with
    | Pane.Heap ->
      (* the heap is the top of the right column: its bottom seam is the rule
         it shares with the flame drawer, and the column divider carries on
         down past both corners it makes *)
      focus_outline
        layout.heap
        ~top:layout.top_divider.y
        ~bottom:layout.heap_divider.y
        ~joints:
          [ seam, layout.top_divider.y, "┬"
          ; seam, layout.row_divider.y, "┤"
          ; seam, layout.heap_divider.y, "├"
          ]
    | Pane.Stack ->
      (* the stack is the top half of the left column: its bottom seam is the
         rule it shares with the source, and the column divider carries on
         down past the corner where they meet *)
      focus_outline
        layout.stack
        ~top:layout.top_divider.y
        ~bottom:layout.row_divider.y
        ~joints:
          [ seam, layout.top_divider.y, "┬"
          ; seam, layout.row_divider.y, "┤"
          ]
    | Pane.Flame ->
      (* the drawer is the bottom of the right column, so it is fenced by the
         heap's rule above and the session bar's below *)
      focus_outline
        layout.flame
        ~top:layout.heap_divider.y
        ~bottom:layout.bottom_divider.y
        ~joints:
          [ seam, layout.heap_divider.y, "├"
          ; seam, layout.bottom_divider.y, "┴"
          ]
  in
  let transport_view =
    Transport.view
      ~width:dimensions.Dimensions.width
      ~step:model.step
      ~total:(Replay.length replay)
      ~density
      ~playing:model.playing
      ~accordion:model.accordion
      ~diagram:(Option.is_some model.popped_out)
      ~flame:flame_chip
  in
  (* The diagram of the row [Enter] was pressed on, over everything else. It
     is a slab rather than a pane, so it is inset from the screen's edges
     instead of taking a place in the layout — the panes it covers are still
     there, and closing it puts them back untouched. It draws from the
     unfiltered registry: the [/] filter thins what you are choosing FROM,
     not what a structure is made of. *)
  let pop_out =
    match model.popped_out with
    | None -> []
    | Some spot ->
      let id = Heap_pane.structure_of_spot spot in
      (match
         List.find structures ~f:(fun (structure : Replay.Structure.t) ->
           structure.id = id)
       with
       | None -> []
       | Some structure ->
         let width =
           Int.min dimensions.width (Int.max 24 (dimensions.width - 8))
         in
         let height =
           Int.min dimensions.height (Int.max 6 (dimensions.height - 4))
         in
         [ View.pad
             ~l:((dimensions.width - width) / 2)
             ~t:((dimensions.height - height) / 2)
             (Heap_pane.Diagram.view
                ~structure
                ~structures
                ~nodes
                ~new_addresses
                ~width
                ~height
                ~scroll:model.pop_scroll
                ~pan:model.pop_pan)
         ])
  in
  let view =
    View.zcat
      (pop_out
       (* the focus seams sit on top of every rule and junction they cross *)
       @ focus_views
       @ [ transport_view
         ; place
             layout.stack
             (Stack_pane.view
                ~width:layout.stack.width
                ~height:layout.stack.height
                ~calls
                ~heat
                ~live
                ~selected
                ~folds:model.stack_folds
                ~cursor:model.stack_cursor
                ~expanded:model.stack_expanded
                ~registered
                ~scroll:model.stack_scroll
                ~collapsed:model.stack_collapsed)
         ; place
             layout.source
             (Source_pane.view
                ~width:layout.source.width
                ~height:layout.source.height
                ~file_label:(Filename.basename file_path)
                ~source
                ~folds:source_folds
                ~active_line:(Location.line_number location)
                ~callsite_line
                ~char_range:(Location.char_range location)
                ~collapsed:model.source_collapsed)
         ; place
             layout.heap
             (Heap_pane.view
                ~note:heap_note
                ~total:(Some (List.length structures))
                ~width:layout.heap.width
                ~height:layout.heap.height
                ~structures:heap_structures
                ~nodes
                ~new_addresses
                ~folds:heap_folds
                ~scroll:model.heap_scroll
                ~selection)
         ; place
             layout.flame
             (Flame_pane.view
                ~width:layout.flame.width
                ~height:layout.flame.height
                ~open_:panel.open_
                ~tree:flame
                ~live:flame_live
                ~zoom:panel.zoom
                ~cursor:panel.cursor
                ~depth_scroll:panel.depth_scroll)
         ; (* junctions ride over the rules they interrupt *)
           View.pad
             ~l:layout.column_divider.x
             ~t:layout.top_divider.y
             (Panel.junction ~color:Theme.border "┬")
         ; View.pad
             ~l:layout.column_divider.x
             ~t:layout.row_divider.y
             (Panel.junction ~color:Theme.border "┤")
         ; View.pad
             ~l:layout.column_divider.x
             ~t:layout.heap_divider.y
             (Panel.junction ~color:Theme.border "├")
         ; View.pad
             ~l:layout.column_divider.x
             ~t:layout.bottom_divider.y
             (Panel.junction ~color:Theme.border "┴")
         ; View.pad
             ~t:layout.top_divider.y
             (Panel.horizontal_rule
                ~width:layout.top_divider.width
                ~color:Theme.border)
         ; View.pad
             ~t:layout.bottom_divider.y
             (Panel.horizontal_rule
                ~width:layout.bottom_divider.width
                ~color:Theme.border)
         ; View.pad
             ~t:layout.row_divider.y
             (Panel.horizontal_rule
                ~width:layout.row_divider.width
                ~color:Theme.border)
         ; View.pad
             ~l:layout.heap_divider.x
             ~t:layout.heap_divider.y
             (Panel.horizontal_rule
                ~width:layout.heap_divider.width
                ~color:Theme.border)
         ; View.pad
             ~l:layout.column_divider.x
             ~t:layout.column_divider.y
             (Panel.vertical_rule
                ~height:layout.column_divider.height
                ~color:Theme.border)
         ; View.pad
             ~t:layout.session.y
             (Session_bar.view
                ~width:dimensions.width
                ~dump_name
                ~heat:
                  (match Array.exists heat ~f:Option.is_some with
                   | false -> None
                   | true -> Some heat_source)
                ~structure:
                  ((* the walked structure's kind, typed when the wire says *)
                   let kind = Snapshot.Ds_type.display snapshot.ds_type in
                   let current =
                     List.find structures ~f:(fun (s : Replay.Structure.t) ->
                       s.is_current)
                   in
                   match current with
                   | Some { ty = Some ty; _ } ->
                     [%string "%{kind} %{Type_info.display ty}"]
                   | Some { ty = None; _ } | None -> kind))
         ; View.rectangle
             ~attrs:[ Attr.bg Theme.bg ]
             ~width:dimensions.width
             ~height:dimensions.height
             ()
         ])
  in
  let on_click (position : Position.t) : [ `Act of Action.t | `Quit ] option =
    let act action = `Act action in
    (* everything below the pane titles — the body hit-tests only start under
       the title row *)
    let titleless () =
      match Region.contains layout.ticks position with
      | true ->
        Transport.step_at
          ~width:layout.ticks.width
          ~total:(Replay.length replay)
          ~x:position.x
        |> Option.map ~f:(fun step -> act (Action.Step_to step))
      | false ->
        (* the drawer's title row is its handle: a click anywhere on it opens
           or shuts it, which is the only way in while it is shut *)
        (match
           ( Region.contains layout.flame position
           , Layout.inner_position layout.flame position )
         with
         | true, None -> Some (act Action.Toggle_flame)
         | true, Some { x; y } ->
           Flame_pane.bar_at
             ~width:layout.flame.width
             ~height:layout.flame.height
             ~tree:flame
             ~live:flame_live
             ~zoom:panel.zoom
             ~cursor:panel.cursor
             ~depth_scroll:panel.depth_scroll
             ~x
             ~row:y
           |> Option.map ~f:(fun path -> act (Action.Jump_flame path))
         | false, (Some _ | None) ->
           (match Layout.inner_position layout.stack position with
            | Some { x; y } ->
              Stack_pane.target_at
                ~width:layout.stack.width
                ~height:layout.stack.height
                ~calls
                ~heat
                ~live
                ~selected
                ~folds:model.stack_folds
                ~cursor:model.stack_cursor
                ~expanded:model.stack_expanded
                ~registered
                ~scroll:model.stack_scroll
                ~x
                ~row:y
              |> Option.map ~f:(fun target ->
                match (target : Stack_pane.Target.t) with
                | Frame index -> act (Action.Select_frame index)
                | Step step -> act (Action.Step_to step)
                | Toggle call -> act (Action.Toggle_stack_fold call)
                | Expand head -> act (Action.Toggle_stack_run head))
            | None ->
              (match Layout.inner_position layout.source position with
               | Some { x; y } ->
                 Source_pane.toggle_at
                   ~width:layout.source.width
                   ~height:layout.source.height
                   ~source
                   ~folds:source_folds
                   ~active_line:(Location.line_number location)
                   ~callsite_line
                   ~char_range:(Location.char_range location)
                   ~x
                   ~y
                 |> Option.map ~f:(fun line ->
                   act
                     (Action.Toggle_source_fold
                        { Source_fold.file = file_path; line }))
               | None ->
                 (match Layout.inner_position layout.heap position with
                  | Some { x; y } ->
                    (* the panel pads the body one column right of the
                       border; fold glyphs win over the row under them *)
                    let x = max 0 (x - 1) in
                    (match
                       Heap_pane.toggle_at
                         ~structures:heap_structures
                         ~nodes
                         ~new_addresses
                         ~folds:heap_folds
                         ~scroll:model.heap_scroll
                         ~selection
                         ~width:layout.heap.width
                         ~height:layout.heap.height
                         ~x
                         ~y
                     with
                     | Some fold -> Some (act (Action.Toggle_heap_fold fold))
                     | None ->
                       Heap_pane.spot_at
                         ~structures:heap_structures
                         ~nodes
                         ~new_addresses
                         ~folds:heap_folds
                         ~scroll:model.heap_scroll
                         ~selection
                         ~width:layout.heap.width
                         ~height:layout.heap.height
                         ~x
                         ~y
                       |> Option.map ~f:(fun spot ->
                         act (Action.Select_heap_node spot)))
                  | None -> None))))
    in
    match Option.is_some model.popped_out with
    (* while the slab is up a click anywhere puts it away, including on the
       [⏎ diagram] chip that raised it — one rule, and it makes the chip the
       toggle its lit state says it is *)
    | true -> Some (act Action.Dismiss_pop_out)
    | false ->
      (match Region.contains layout.controls position with
       | true ->
         Transport.control_at
           ~width:layout.controls.width
           ~playing:model.playing
           ~flame:flame_chip
           ~x:position.x
         |> Option.map ~f:(fun button ->
           match (button : Transport.Button.t) with
           | Back -> act (Action.Step_delta (-1))
           | Step -> act (Action.Step_delta 1)
           | Play -> act Action.Toggle_play
           | Latest -> act Action.Focus_latest
           | Node -> act (Action.Move_cursor Down)
           | Diagram -> act Action.Pop_out
           | Fold -> act Action.Toggle_focused_fold
           | Accordion -> act Action.Toggle_accordion
           | Filter -> act Action.Begin_filter
           | Flame -> act Action.Toggle_flame
           | Zoom -> act Action.Zoom_flame
           | Reset_zoom -> act Action.Reset_flame_zoom
           | Quit -> `Quit)
       | false ->
         (* the pane titles collapse and reopen their pane; the flame
            drawer's title is handled with its body, in [titleless], where a
            click on it opens and shuts the drawer *)
         (match Layout.on_title layout.stack position with
          | true -> Some (act Action.Toggle_stack_pane)
          | false ->
            (match Layout.on_title layout.source position with
             | true -> Some (act Action.Toggle_source_pane)
             | false -> titleless ())))
  in
  (* The wheel has one axis, so the diagram's sideways rides on a held
     modifier, with [\[]/[\]] for the same thing from the keyboard.

     Ctrl or alt, NOT shift. The terminal's mouse encoding does carry a shift
     bit, but notty decodes only the meta and ctrl ones
     ([Notty.Unescape.mouse_p] reads bits 3 and 4 and no others), so
     shift+wheel arrives here as a bare wheel event with nothing to tell it
     apart from an unmodified one. Notty drops the dedicated horizontal-wheel
     buttons too, so there is no second axis to read either. *)
  let on_scroll (position : Position.t) direction ~sideways : Action.t option
    =
    let delta = match direction with `Up -> -1 | `Down -> 1 in
    match Option.is_some model.popped_out, sideways with
    | true, false -> Some (Action.Scroll_pop_out delta)
    (* four columns a tick, roughly a wheel notch's share of a box *)
    | true, true -> Some (Action.Pan_pop_out (delta * 4))
    (* no pane has anything sideways to reach — the outline's rows wrap, the
       stack centers itself, and a flame bar is already scaled to the width
       it is given — so a held modifier changes nothing: over the drawer the
       wheel walks depth, elsewhere it scrolls *)
    | false, (true | false) ->
      (match
         ( Region.contains layout.flame position
         , Region.contains layout.heap position
         , Region.contains layout.stack position )
       with
       | true, (_ : bool), (_ : bool) -> Some (Action.Scroll_flame delta)
       | false, true, (_ : bool) -> Some (Action.Scroll_heap delta)
       | false, false, true -> Some (Action.Scroll_stack delta)
       | false, false, false -> None)
  in
  { Computed.view; on_click; on_scroll }
;;

let component
  ?profile
  ~replay
  ~sources
  ~dump_name
  ~exit
  ~dimensions
  (local_ graph)
  =
  let births = birth_steps replay in
  let calls =
    Array.init (Replay.length replay) ~f:(fun step ->
      (Replay.step_exn replay ~step).call)
  in
  let heat, heat_source = heat_of_calls ~profile ~calls in
  (* the whole run folded into bars, once: the tree is over the trace, not
     over a step, so it never changes while the replay runs *)
  let flame = Flame_tree.create ~calls ~profile in
  let density = density_of_steps replay in
  let registered = registrations replay in
  let model, inject =
    (* [with_input] only for [dimensions]: the landing scroll and the flame
       drawer's cursor both follow the picture actually drawn, and that
       depends on the terminal's size *)
    Bonsai.state_machine_with_input
      ~sexp_of_model:Model.sexp_of_t
      ~sexp_of_action:Action.sexp_of_t
      ~equal:Model.equal
      ~default_model:Model.initial
      ~apply_action:(apply_action replay ~calls ~births ~flame)
      dimensions
      graph
  in
  let tick =
    let%arr { Model.playing; _ } = model
    and inject in
    match playing with true -> inject Action.Tick | false -> Effect.Ignore
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
    ~trigger_on_activate:false
    (Bonsai.return play_interval)
    tick
    graph;
  let computed =
    let%arr model and dimensions in
    render
      ~replay
      ~sources
      ~dump_name
      ~calls
      ~heat
      ~heat_source
      ~density
      ~registered
      ~flame
      ~model
      ~dimensions
  in
  let view =
    let%arr { Computed.view; _ } = computed in
    view
  in
  let handler =
    let%arr { Computed.on_click; on_scroll; view = _ } = computed
    and { Model.typing_filter; popped_out; flame = panel; focus; _ } = model
    and inject in
    let inject_or_ignore action =
      match action with
      | Some action -> inject action
      | None -> Effect.Ignore
    in
    (* the drawer only takes the keyboard when [Tab] has actually put it
       there: it shares the screen with the heap, so both must stay drivable *)
    let flame_focused = panel.Flame.open_ && Pane.equal focus Pane.Flame in
    let click_or_ignore click =
      match click with
      | Some (`Act action) -> inject action
      | Some `Quit -> exit ()
      | None -> Effect.Ignore
    in
    fun (event : Event.t) ->
      match event with
      | Key_press { key; mods }
        when match key with
             | ASCII ('c' | 'C') ->
               List.mem mods Event.Modifier.Ctrl ~equal:Event.Modifier.equal
             | _ -> false ->
        exit ()
      (* The pop-out owns the keyboard while it is up: [Escape] backs out and
         [Enter] goes through — which is what those two keys mean everywhere
         — and the rest moves around a diagram bigger than the slab showing
         it. Nothing steps, because the slab is a look at one step of the
         run: to carry on you close it first. *)
      | Key_press { key; mods } when Option.is_some popped_out ->
        (match key, mods with
         | Escape, [] -> inject Dismiss_pop_out
         | Enter, [] -> inject Commit_pop_out
         | (Arrow `Up | ASCII 'w'), [] -> inject (Scroll_pop_out (-1))
         | (Arrow `Down | ASCII 's'), [] -> inject (Scroll_pop_out 1)
         | (Arrow `Left | ASCII 'a'), [] -> inject (Pan_pop_out (-4))
         | (Arrow `Right | ASCII 'd'), [] -> inject (Pan_pop_out 4)
         | ASCII '[', [] -> inject (Pan_pop_out (-8))
         | ASCII ']', [] -> inject (Pan_pop_out 8)
         | Page `Up, [] -> inject (Scroll_pop_out (-3))
         | Page `Down, [] -> inject (Scroll_pop_out 3)
         | ASCII 'q', [] -> exit ()
         | _ -> Effect.Ignore)
      (* while the [/] prompt is open it owns the keyboard: the letters that
         would otherwise aim, play or quit spell the filter instead *)
      | Key_press { key; mods } when typing_filter ->
        (match key, mods with
         | Enter, [] -> inject Commit_filter
         | Escape, [] -> inject Cancel_filter
         | Backspace, [] -> inject Filter_backspace
         | ASCII char, [] when Char.is_print char ->
           inject (Filter_input char)
         | _ -> Effect.Ignore)
      | Key_press { key; mods } ->
        (* the key map the footer advertises; anything else is ignored *)
        (match key, mods with
         (* [f] opens and shuts the drawer from anywhere. The rest only bite
            while [Tab] has left the keyboard in it, because the heap is
            still on screen beside it and has to stay drivable — [wasd], [↑↓]
            and [Enter] already dispatch on focus, and [z] and [PgUp]/[PgDn]
            join them here: accordion and heap scroll elsewhere, zoom and
            depth in the drawer. *)
         | ASCII 'f', [] -> inject Toggle_flame
         | ASCII 'z', [] when flame_focused -> inject Zoom_flame
         | ASCII 'Z', [] when flame_focused -> inject Reset_flame_zoom
         | Page `Up, [] when flame_focused -> inject (Scroll_flame (-1))
         | Page `Down, [] when flame_focused -> inject (Scroll_flame 1)
         | ASCII 'q', [] -> exit ()
         | (Arrow `Right | ASCII ('l' | 'n')), [] -> inject (Step_delta 1)
         | (Arrow `Left | ASCII 'p'), [] -> inject (Step_delta (-1))
         | ASCII ' ', [] -> inject Toggle_play
         (* [h] gives up stepping back — ← and [p] still do that — to fold
            whatever the focused pane is pointing at *)
         | ASCII 'h', [] -> inject Toggle_focused_fold
         | ASCII 'z', [] -> inject Toggle_accordion
         | ASCII '.', [] -> inject Focus_latest
         (* the left column's panes, top to bottom *)
         | ASCII '1', [] -> inject Toggle_stack_pane
         | ASCII '2', [] -> inject Toggle_source_pane
         | ASCII 'o', [] -> inject Toggle_address_order
         | ASCII '/', [] -> inject Begin_filter
         (* clears a committed filter without reopening the prompt *)
         | Escape, [] -> inject Cancel_filter
         | (Home | ASCII 'g'), [] -> inject (Step_to 0)
         | (End | ASCII 'G'), [] -> inject (Step_to Int.max_value)
         | Page `Up, [] -> inject (Scroll_heap (-3))
         | Page `Down, [] -> inject (Scroll_heap 3)
         | Tab, [] -> inject Focus_next_pane
         (* [Enter] in the heap pops the diagram out — the outline is the
            everyday reading and this is the other one, a keystroke away. In
            the stack and the flame drawer it still commits, where there is
            no diagram to want and a frame or a bar's first call to jump to.
            Clicking a heap row commits it either way. *)
         | Enter, [] ->
           (match focus with
            | Pane.Heap -> inject Pop_out
            | Pane.Stack | Pane.Flame -> inject Commit_cursor)
         (* the outline is a list of lines, so ↑/↓ run it end to end — the
            motion the heap pane is shaped for, on the keys that mean it. ←/→
            stay on stepping, which is what a replay is for. *)
         | Arrow `Up, [] -> inject (Move_cursor Up)
         | Arrow `Down, [] -> inject (Move_cursor Down)
         (* lowercase aims, uppercase commits on the way — a terminal reports
            shift as the capital, not as a modifier *)
         | ASCII 'w', [] -> inject (Move_cursor Up)
         | ASCII 's', [] -> inject (Move_cursor Down)
         | ASCII 'a', [] -> inject (Move_cursor Left)
         | ASCII 'd', [] -> inject (Move_cursor Right)
         | ASCII 'W', [] -> inject (Jump_cursor Up)
         | ASCII 'S', [] -> inject (Jump_cursor Down)
         | ASCII 'A', [] -> inject (Jump_cursor Left)
         | ASCII 'D', [] -> inject (Jump_cursor Right)
         | _ -> Effect.Ignore)
      | Mouse { kind = Left; position; mods = _ } ->
        click_or_ignore (on_click position)
      (* any modifier at all means sideways: notty only ever reports ctrl and
         meta for a mouse event, which are exactly the two that can mean it *)
      | Mouse { kind = Scroll direction; position; mods } ->
        inject_or_ignore
          (on_scroll position direction ~sideways:(not (List.is_empty mods)))
      | Mouse _ | Paste _ -> Effect.Ignore
  in
  ~view, ~handler
;;

let run ?profile ~dump_name ~replay ~sources () =
  Bonsai_term.start_with_exit (fun ~exit ~dimensions (local_ graph) ->
    component ?profile ~replay ~sources ~dump_name ~exit ~dimensions graph)
;;
