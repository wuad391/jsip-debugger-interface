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

module Model = struct
  type t =
    { step : int
    ; selected_frame : int option (** [None] = innermost frame *)
    ; playing : bool
    ; heap_scroll : int
    ; heap_folds : Set.M(Heap_pane.Fold).t
    ; stack_folds : Int.Set.t
    ; source_folds : Set.M(Source_fold).t
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
  (* any move re-follows the innermost frame and rewinds the heap pane; folds
     persist — that is the point of keying them stably *)
  let move ~playing step =
    { model with
      step = clamp_step step
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
  | Scroll_heap delta ->
    { model with heap_scroll = Int.max 0 (model.heap_scroll + delta) }
  | Toggle_heap_fold fold ->
    { model with
      heap_folds =
        (match Set.mem model.heap_folds fold with
         | true -> Set.remove model.heap_folds fold
         | false -> Set.add model.heap_folds fold)
    }
  | Toggle_stack_fold call ->
    { model with
      stack_folds =
        (match Set.mem model.stack_folds call with
         | true -> Set.remove model.stack_folds call
         | false -> Set.add model.stack_folds call)
    }
  | Toggle_source_fold fold ->
    { model with
      source_folds =
        (match Set.mem model.source_folds fold with
         | true -> Set.remove model.source_folds fold
         | false -> Set.add model.source_folds fold)
    }
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

let render
  ~replay
  ~sources
  ~dump_name
  ~births
  ~calls
  ~(model : Model.t)
  ~dimensions
  =
  let layout = Layout.compute dimensions in
  let { Replay.Step.call
      ; frames
      ; structures
      ; nodes
      ; new_addresses
      ; description = _
      }
    =
    Replay.step_exn replay ~step:model.step
  in
  let count = List.length frames in
  (* a frame's own event index is the start of its range *)
  let live = List.map frames ~f:(fun (frame : Call.t) -> fst frame.range) in
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
  let place (region : Region.t) view =
    View.pad ~l:region.x ~t:region.y view
  in
  let view =
    View.zcat
      [ Transport.view
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
             ~folds:model.stack_folds)
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
             ~width:layout.heap.width
             ~height:layout.heap.height
             ~structures
             ~nodes
             ~new_addresses
             ~folds:model.heap_folds
             ~scroll:model.heap_scroll)
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
          ~t:layout.top_divider.y
          (Panel.horizontal_rule
             ~width:layout.top_divider.width
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
      ]
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
                       ~structures
                       ~nodes
                       ~new_addresses
                       ~folds:model.heap_folds
                       ~scroll:model.heap_scroll
                       ~height:layout.heap.height
                       ~x
                       ~y
                   with
                   | Some fold -> Some (act (Action.Toggle_heap_fold fold))
                   | None ->
                     Heap_pane.address_at
                       ~structures
                       ~nodes
                       ~new_addresses
                       ~folds:model.heap_folds
                       ~scroll:model.heap_scroll
                       ~height:layout.heap.height
                       ~x
                       ~y
                     |> Option.bind ~f:(Map.find births)
                     |> Option.map ~f:(fun step -> act (Action.Step_to step)))
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
    render ~replay ~sources ~dump_name ~births ~calls ~model ~dimensions
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
    let click_or_ignore click =
      match click with
      | Some (`Act action) -> inject action
      | Some `Quit -> exit ()
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
         | (Home | ASCII 'g'), [] -> inject (Step_to 0)
         | (End | ASCII 'G'), [] -> inject (Step_to Int.max_value)
         | Page `Up, [] -> inject (Scroll_heap (-3))
         | Page `Down, [] -> inject (Scroll_heap 3)
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
