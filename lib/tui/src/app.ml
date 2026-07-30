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

module Model = struct
  type t =
    { step : int
    ; selected_frame : int option (** [None] = innermost frame *)
    ; playing : bool
    ; heap_scroll : int
    }
  [@@deriving sexp_of, equal]

  let initial =
    { step = 0; selected_frame = None; playing = false; heap_scroll = 0 }
  ;;
end

module Action = struct
  type t =
    | Step_to of int
    | Step_delta of int
    | Tick
    | Toggle_play
    | Select_frame of int
    | Move_frame of int
    | Scroll_heap of int
  [@@deriving sexp_of]
end

let frame_count replay ~step =
  List.length (Replay.step_exn replay ~step).frames
;;

let apply_action
  replay
  (_ : _ Bonsai.Apply_action_context.t)
  (model : Model.t)
  action
  =
  let last = Replay.length replay - 1 in
  let clamp_step step = Int.max 0 (Int.min last step) in
  let clamp_frame index =
    Int.max 0 (Int.min (frame_count replay ~step:model.step - 1) index)
  in
  (* any move re-follows the innermost frame and rewinds the heap pane *)
  let move ~playing step =
    { Model.step = clamp_step step
    ; selected_frame = None
    ; playing
    ; heap_scroll = 0
    }
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
  | Move_frame delta ->
    let current =
      Option.value
        model.selected_frame
        ~default:(frame_count replay ~step:model.step - 1)
    in
    { model with selected_frame = Some (clamp_frame (current + delta)) }
  | Scroll_heap delta ->
    { model with heap_scroll = Int.max 0 (model.heap_scroll + delta) }
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

let phase replay ~step =
  let depth_at step = (Replay.step_exn replay ~step).call.info.depth in
  match step with
  | 0 -> "setup"
  | step ->
    (match
       Ordering.of_int (compare (depth_at step) (depth_at (step - 1)))
     with
     | Greater -> "descend"
     | Less -> "unwind"
     | Equal -> "step")
;;

module Computed = struct
  type t =
    { view : View.t
    ; on_click : Position.t -> Action.t option
    ; on_scroll : Position.t -> [ `Up | `Down ] -> Action.t option
    }
end

let render ~replay ~sources ~dump_name ~births ~(model : Model.t) ~dimensions
  =
  let layout = Layout.compute dimensions in
  let { Replay.Step.call; frames; new_addresses; description } =
    Replay.step_exn replay ~step:model.step
  in
  let count = List.length frames in
  let selected =
    Int.max
      0
      (Int.min
         (count - 1)
         (Option.value model.selected_frame ~default:(count - 1)))
  in
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
  let registry = call.info.registry in
  let snapshot = call.info.snapshot in
  let place (region : Region.t) view =
    View.pad ~l:region.x ~t:region.y view
  in
  let view =
    View.zcat
      [ place
          layout.top_bar
          (Top_bar.view
             ~width:layout.top_bar.width
             ~dump_name
             ~structure:(Snapshot.Ds_type.display snapshot.ds_type)
             ~phase:(phase replay ~step:model.step)
             ~step:(model.step + 1)
             ~total:(Replay.length replay))
      ; place
          layout.stack
          (Stack_pane.view
             ~width:layout.stack.width
             ~height:layout.stack.height
             ~frames
             ~selected)
      ; place
          layout.source
          (Source_pane.view
             ~width:layout.source.width
             ~height:layout.source.height
             ~file_label:(Filename.basename (Location.file_path location))
             ~source
             ~active_line:(Location.line_number location)
             ~callsite_line
             ~char_range:(Location.char_range location))
      ; place
          layout.heap
          (Heap_pane.view
             ~width:layout.heap.width
             ~height:layout.heap.height
             ~snapshot
             ~registry
             ~new_addresses
             ~scroll:model.heap_scroll)
      ; View.pad
          ~t:(layout.ticks.y - 1)
          (Footer.view
             ~width:dimensions.Dimensions.width
             ~step:model.step
             ~total:(Replay.length replay)
             ~playing:model.playing
             ~status:description)
      ; View.rectangle
          ~attrs:[ Attr.bg Theme.bg ]
          ~width:dimensions.width
          ~height:dimensions.height
          ()
      ]
  in
  let on_click (position : Position.t) : Action.t option =
    match Region.contains layout.controls position with
    | true ->
      Footer.button_at ~x:position.x
      |> Option.map ~f:(fun button ->
        match (button : Footer.Button.t) with
        | Back -> Action.Step_delta (-1)
        | Step -> Action.Step_delta 1
        | Play -> Action.Toggle_play)
    | false ->
      (match Region.contains layout.ticks position with
       | true ->
         Footer.step_at
           ~width:layout.ticks.width
           ~total:(Replay.length replay)
           ~x:position.x
         |> Option.map ~f:(fun step -> Action.Step_to step)
       | false ->
         (match Layout.inner_position layout.stack position with
          | Some { x = _; y } ->
            Stack_pane.frame_at
              ~height:layout.stack.height
              ~frames:count
              ~selected
              ~row:y
            |> Option.map ~f:(fun index -> Action.Select_frame index)
          | None ->
            (match Layout.inner_position layout.heap position with
             | Some { x = _; y } ->
               Heap_pane.address_at
                 ~snapshot
                 ~registry
                 ~new_addresses
                 ~scroll:model.heap_scroll
                 ~height:layout.heap.height
                 ~row:y
               |> Option.bind ~f:(Map.find births)
               |> Option.map ~f:(fun step -> Action.Step_to step)
             | None -> None)))
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
  let model, inject =
    Bonsai.state_machine
      ~sexp_of_model:Model.sexp_of_t
      ~sexp_of_action:Action.sexp_of_t
      ~equal:Model.equal
      ~default_model:Model.initial
      ~apply_action:(apply_action replay)
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
    render ~replay ~sources ~dump_name ~births ~model ~dimensions
  in
  let view =
    let%arr { Computed.view; _ } = computed in
    view
  in
  let handler =
    let%arr { Computed.on_click; on_scroll; view = _ } = computed
    and inject in
    let inject_or_ignore action =
      match action with
      | Some action -> inject action
      | None -> Effect.Ignore
    in
    fun (event : Event.t) ->
      match event with
      | Key_press { key; mods } ->
        (* the key map the footer advertises; anything else is ignored *)
        (match key, mods with
         | ASCII ('c' | 'C'), mods
           when List.mem mods Event.Modifier.Ctrl ~equal:Event.Modifier.equal
           ->
           exit ()
         | ASCII 'q', [] -> exit ()
         | (Arrow `Right | ASCII ('l' | 'n')), [] -> inject (Step_delta 1)
         | (Arrow `Left | ASCII ('h' | 'p')), [] -> inject (Step_delta (-1))
         | ASCII ' ', [] -> inject Toggle_play
         | Arrow `Up, [] -> inject (Move_frame (-1))
         | Arrow `Down, [] -> inject (Move_frame 1)
         | (Home | ASCII 'g'), [] -> inject (Step_to 0)
         | (End | ASCII 'G'), [] -> inject (Step_to Int.max_value)
         | Page `Up, [] -> inject (Scroll_heap (-3))
         | Page `Down, [] -> inject (Scroll_heap 3)
         | _ -> Effect.Ignore)
      | Mouse { kind = Left; position; mods = _ } ->
        inject_or_ignore (on_click position)
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
