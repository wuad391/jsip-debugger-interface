open! Core
open Bonsai.Let_syntax
open Jsip_types
open Jsip_replay
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
   to select, so it is not in the rotation. *)
module Pane = struct
  type t =
    | Stack
    | Heap
  [@@deriving sexp_of, equal]

  let other t = match t with Stack -> Heap | Heap -> Stack
end

module Model = struct
  type t =
    { step : int
    ; selected_frame : int option (** [None] = innermost frame *)
    ; playing : bool
    ; heap_scroll : int
    ; heap_folds : Set.M(Heap_pane.Fold).t
    ; stack_folds : Int.Set.t
    ; source_folds : Set.M(Source_fold).t
    ; focus : Pane.t
    ; heap_selected : Heap_pane.Spot.t option
    (** [None] falls back to the structure this step walked, which is what
        the pane highlighted before selection existed *)
    ; heap_cursor : Heap_pane.Spot.t option
    ; stack_cursor : int option (** a call index *)
    ; accordion : bool
    (** [z]: every structure collapsed but the one the keyboard is in *)
    ; heap_filter : string
    (** [/]: only structures whose header matches stay on the canvas. Empty =
        no filter. *)
    ; typing_filter : bool
    (** the [/] prompt is open, and keystrokes edit the filter instead of
        driving the panes *)
    }
  [@@deriving sexp_of, equal]

  let initial =
    { step = 0
    ; selected_frame = None
    ; playing = false
    ; heap_scroll = 0
    ; heap_folds = Set.empty (module Heap_pane.Fold)
    ; stack_folds = Int.Set.empty
    ; source_folds = Set.empty (module Source_fold)
    ; focus = Pane.Heap
    ; heap_selected = None
    ; heap_cursor = None
    ; stack_cursor = None
    ; accordion = false
    ; heap_filter = ""
    ; typing_filter = false
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
    | Toggle_heap_fold of Heap_pane.Fold.t
    | Toggle_stack_fold of int
    | Toggle_source_fold of Source_fold.t
    | Focus_next_pane
    | Move_cursor of Heap_pane.Direction.t
    (** [wasd]: aim, without committing *)
    | Commit_cursor (** [Enter]: the orange becomes the blue *)
    | Jump_cursor of Heap_pane.Direction.t
    (** shift + [wasd]: aim and commit in one, skipping the orange *)
    | Select_heap_node of Heap_pane.Spot.t (** a click on a card *)
    | Toggle_focused_fold
    (** [space]: collapse whatever the focused pane is pointing at — the
        structure a heap card belongs to, or a call's descendants *)
    | Toggle_accordion
    | Begin_filter (** [/]: open the prompt, starting from empty *)
    | Filter_input of char
    | Filter_backspace
    | Commit_filter (** [Enter]: close the prompt, keep the filter *)
    | Cancel_filter (** [Escape]: close the prompt, drop the filter *)
  [@@deriving sexp_of]
end

let frame_count replay ~step =
  List.length (Replay.step_exn replay ~step).frames
;;

(* Indices into a list that can be empty, so [max] is [-1] there and
   [Int.clamp_exn] would assert rather than clamp. *)
let clamp index ~max = Int.max 0 (Int.min max index)

(* the blue card: whatever was last chosen, or — until something is — the
   root of the structure this step walked, which is what the pane highlighted
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
   the structures whose header matches, and accordion mode closes every
   structure but the one the keyboard is in. The renderer, the hit-tests and
   the cursor arithmetic all go through here so they cannot disagree about
   what is on the canvas. *)
let heap_inputs replay (model : Model.t) =
  let structures =
    List.filter
      (Replay.step_exn replay ~step:model.step).structures
      ~f:(fun structure ->
        Heap_pane.matches_filter structure ~filter:model.heap_filter)
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
    fst frame.range)
;;

let apply_action
  replay
  ~calls
  ~births
  (_ : _ Bonsai.Apply_action_context.t)
  (model : Model.t)
  action
  =
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
  (* any move re-follows the innermost frame and rewinds the heap pane, and
     drops both cursors and the chosen card — at another step those addresses
     name nothing, so blue goes back to following the walked structure. Folds
     persist; that is the point of keying them stably. *)
  let move ~playing step =
    { model with
      step = clamp_step step
    ; selected_frame = None
    ; playing
    ; heap_scroll = 0
    ; heap_selected = None
    ; heap_cursor = None
    ; stack_cursor = None
    }
  in
  (* committing a heap card is exactly what clicking it does — jump to where
     it was allocated — and additionally pins it as the selection, so the
     card stays blue and keeps showing its address at the new step *)
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
    { stepped with heap_selected = selected; heap_cursor = None }
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
          | Stack_pane.Target.Toggle (_ : int) -> model))
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
             ~direction
         in
         (match moved with
          | None -> model
          | Some call -> { model with stack_cursor = Some call }))
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
    { model with selected_frame = Some (clamp_frame index) }
  | Scroll_heap delta ->
    { model with heap_scroll = Int.max 0 (model.heap_scroll + delta) }
  | Toggle_heap_fold fold -> toggle_heap_fold model fold
  | Toggle_stack_fold call ->
    { model with stack_folds = toggle model.stack_folds call }
  | Toggle_source_fold fold ->
    { model with source_folds = toggle model.source_folds fold }
  (* [h] folds what you are pointing at: in the heap the whole structure the
     cursor's card belongs to, in the stack the aimed call's range. It reads
     the cursor first and the selection second, so it works before you have
     aimed at anything — the heap falls back to the structure this step
     walked, the stack to the selected frame. *)
  | Toggle_focused_fold ->
    (match model.focus with
     | Pane.Heap ->
       let { Heap_pane.Selection.selected; cursor } =
         heap_selection replay model
       in
       (match Option.first_some cursor selected with
        | None -> model
        | Some spot -> toggle_heap_fold model (Heap_pane.fold_of_spot spot))
     | Pane.Stack ->
       let live = live_calls replay ~step:model.step in
       (match
          Option.first_some
            model.stack_cursor
            (List.nth live (selected_frame replay model))
        with
        | None -> model
        | Some call ->
          { model with stack_folds = toggle model.stack_folds call }))
  | Toggle_accordion -> { model with accordion = not model.accordion }
  (* [/] always starts from empty: the old filter was shaped around whatever
     you were hunting last time, and editing it beats out of a prompt this
     small costs more keys than retyping *)
  | Begin_filter -> { model with typing_filter = true; heap_filter = "" }
  | Filter_input char ->
    { model with heap_filter = model.heap_filter ^ String.of_char char }
  | Filter_backspace ->
    { model with heap_filter = String.drop_suffix model.heap_filter 1 }
  | Commit_filter -> { model with typing_filter = false }
  | Cancel_filter -> { model with typing_filter = false; heap_filter = "" }
  | Focus_next_pane -> { model with focus = Pane.other model.focus }
  | Move_cursor direction -> aim model ~direction
  | Commit_cursor -> commit model
  | Jump_cursor direction -> commit (aim model ~direction)
  | Select_heap_node spot -> select_heap_node model spot
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
    ; on_scroll : Position.t -> [ `Up | `Down ] -> Action.t option
    }
end

let render ~replay ~sources ~dump_name ~calls ~(model : Model.t) ~dimensions =
  let layout = Layout.compute dimensions in
  let { Replay.Step.call; frames; structures; nodes; new_addresses } =
    Replay.step_exn replay ~step:model.step
  in
  (* a frame's own event index is the start of its range *)
  let live = List.map frames ~f:(fun (frame : Call.t) -> fst frame.range) in
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
     typed (a block cursor while the prompt is open), the accordion by name *)
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
    match List.filter_opt [ filter; accordion ] with
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
      (* the heap runs the full height between the two full-width rules, so
         its left seam is the column divider — and the stack/source rule dies
         on that divider, which keeps its tee, in orange *)
      focus_outline
        layout.heap
        ~top:layout.top_divider.y
        ~bottom:layout.bottom_divider.y
        ~joints:
          [ seam, layout.top_divider.y, "┬"
          ; seam, layout.row_divider.y, "┤"
          ; seam, layout.bottom_divider.y, "┴"
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
  in
  let view =
    View.zcat
      ((* the focus seams sit on top of every rule and junction they cross *)
       focus_views
       @ [ Transport.view
             ~width:dimensions.Dimensions.width
             ~step:model.step
             ~total:(Replay.length replay)
             ~playing:model.playing
         ; place
             layout.stack
             (Stack_pane.view
                ~width:layout.stack.width
                ~height:layout.stack.height
                ~calls
                ~live
                ~selected
                ~folds:model.stack_folds
                ~cursor:model.stack_cursor)
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
                ~char_range:(Location.char_range location))
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
    match Region.contains layout.controls position with
    | true ->
      Transport.control_at
        ~width:layout.controls.width
        ~playing:model.playing
        ~x:position.x
      |> Option.map ~f:(fun button ->
        match (button : Transport.Button.t) with
        | Back -> act (Action.Step_delta (-1))
        | Step -> act (Action.Step_delta 1)
        | Play -> act Action.Toggle_play
        | Quit -> `Quit)
    | false ->
      (match Region.contains layout.ticks position with
       | true ->
         Transport.step_at
           ~width:layout.ticks.width
           ~total:(Replay.length replay)
           ~x:position.x
         |> Option.map ~f:(fun step -> act (Action.Step_to step))
       | false ->
         (match Layout.inner_position layout.stack position with
          | Some { x; y } ->
            Stack_pane.target_at
              ~width:layout.stack.width
              ~height:layout.stack.height
              ~calls
              ~live
              ~selected
              ~folds:model.stack_folds
              ~cursor:model.stack_cursor
              ~x
              ~row:y
            |> Option.map ~f:(fun target ->
              match (target : Stack_pane.Target.t) with
              | Frame index -> act (Action.Select_frame index)
              | Step step -> act (Action.Step_to step)
              | Toggle call -> act (Action.Toggle_stack_fold call))
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
                  (* the panel pads the body one column right of the border;
                     fold glyphs win over the card under them *)
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
  let on_scroll (position : Position.t) direction : Action.t option =
    match Region.contains layout.heap position with
    | false -> None
    | true ->
      (match direction with
       | `Up -> Some (Action.Scroll_heap (-1))
       | `Down -> Some (Action.Scroll_heap 1))
  in
  { Computed.view; on_click; on_scroll }
;;

let component ~replay ~sources ~dump_name ~exit ~dimensions (local_ graph) =
  let births = birth_steps replay in
  let calls =
    Array.init (Replay.length replay) ~f:(fun step ->
      (Replay.step_exn replay ~step).call)
  in
  let model, inject =
    Bonsai.state_machine
      ~sexp_of_model:Model.sexp_of_t
      ~sexp_of_action:Action.sexp_of_t
      ~equal:Model.equal
      ~default_model:Model.initial
      ~apply_action:(apply_action replay ~calls ~births)
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
    render ~replay ~sources ~dump_name ~calls ~model ~dimensions
  in
  let view =
    let%arr { Computed.view; _ } = computed in
    view
  in
  let handler =
    let%arr { Computed.on_click; on_scroll; view = _ } = computed
    and { Model.typing_filter; _ } = model
    and inject in
    let inject_or_ignore action =
      match action with
      | Some action -> inject action
      | None -> Effect.Ignore
    in
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
         | ASCII 'q', [] -> exit ()
         | (Arrow `Right | ASCII ('l' | 'n')), [] -> inject (Step_delta 1)
         | (Arrow `Left | ASCII 'p'), [] -> inject (Step_delta (-1))
         | ASCII ' ', [] -> inject Toggle_play
         (* [h] gives up stepping back — ← and [p] still do that — to fold
            whatever the focused pane is pointing at *)
         | ASCII 'h', [] -> inject Toggle_focused_fold
         | ASCII 'z', [] -> inject Toggle_accordion
         | ASCII '/', [] -> inject Begin_filter
         (* clears a committed filter without reopening the prompt *)
         | Escape, [] -> inject Cancel_filter
         | (Home | ASCII 'g'), [] -> inject (Step_to 0)
         | (End | ASCII 'G'), [] -> inject (Step_to Int.max_value)
         | Page `Up, [] -> inject (Scroll_heap (-3))
         | Page `Down, [] -> inject (Scroll_heap 3)
         | Tab, [] -> inject Focus_next_pane
         | Enter, [] -> inject Commit_cursor
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
      | Mouse { kind = Scroll direction; position; mods = _ } ->
        inject_or_ignore (on_scroll position direction)
      | Mouse _ | Paste _ -> Effect.Ignore
  in
  ~view, ~handler
;;

let run ~dump_name ~replay ~sources =
  Bonsai_term.start_with_exit (fun ~exit ~dimensions (local_ graph) ->
    component ~replay ~sources ~dump_name ~exit ~dimensions graph)
;;
