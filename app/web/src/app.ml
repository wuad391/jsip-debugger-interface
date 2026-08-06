open! Core
open Jsip_types
open Jsip_replay
open Jsip_web_components
open Bonsai.Let_syntax
module Vdom = Virtual_dom.Vdom
open Vdom.Html_syntax
module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html
module Effect = Bonsai_web.Effect

let play_interval = Time_ns.Span.of_int_ms 850

(* how far the heap's corner slider goes. It counts AVERAGE-sized structures
   across, not columns, so the top of the range is a survey of a whole
   registry at once rather than a grid nobody could read. *)
let max_columns = 16

module Model = struct
  type t =
    { step : int
    ; selected_frame : int option (** [None] = innermost frame *)
    ; playing : bool
    ; land_seq : int
    (** bumped by stepping: brings the walked structure back into the
        canvas's view if it slipped off *)
    ; focus_seq : int
    (** bumped by [.] and the heap header's [⌖]: the canvas zooms to
        {!focus_target} *)
    ; focus_target : Action.Focus_target.t
    (** which mark the next [⌖] goes to; each press alternates *)
    ; stack_folds : Int.Set.t
    ; stack_expanded : Int.Set.t
    ; source_folds : Set.M(Action.Source_fold).t
    ; stack_collapsed : bool
    ; source_collapsed : bool
    ; heap_view : Action.Heap_view.t
    (** which reading the heap pane's tabs are showing — the canvas diagram
        or the outline over the same scene *)
    ; heap_folds : Set.M(Heap_scene.Fold_key).t
    ; accordion : bool
    ; sort_by_address : bool
    ; heap_columns : int
    (** structures across the canvas before the next row of them *)
    ; heap_filter : string
    ; typing_filter : bool
    ; heap_selected : Snapshot.Address.t option
    (** [None] falls back to the structure this step walked *)
    ; lod : Action.Lod.t
    ; edge_style : Action.Edge_style.t
    ; theme_mode : Action.Theme_mode.t
    ; flame_open : bool
    ; flame_zoom : Flame_math.Path.t
    ; hud_zoom_percent : int
    ; hud_tier : float
    }
  [@@deriving sexp_of, equal]

  let initial =
    { step = 0
    ; selected_frame = None
    ; playing = false
    ; land_seq = 0
    ; focus_seq = 0
    ; focus_target = Action.Focus_target.Selection
    ; stack_folds = Int.Set.empty
    ; stack_expanded = Int.Set.empty
    ; source_folds = Set.empty (module Action.Source_fold)
    ; stack_collapsed = false
    ; source_collapsed = false
    ; heap_view = Action.Heap_view.Diagram
    ; heap_folds = Set.empty (module Heap_scene.Fold_key)
    ; accordion = false
    ; sort_by_address = false
    ; heap_columns = 3
    ; heap_filter = ""
    ; typing_filter = false
    ; heap_selected = None
    ; lod = Action.Lod.Uniform
    ; edge_style = Action.Edge_style.Angled
    ; theme_mode = Action.Theme_mode.Dark
    ; flame_open = false
    ; flame_zoom = []
    ; hud_zoom_percent = 100
    ; hud_tier = 1.5
    }
  ;;
end

let frame_count replay ~step =
  List.length (Replay.step_exn replay ~step).frames
;;

(* Indices into a list that can be empty, so [max] is [-1] there and
   [Int.clamp_exn] would assert rather than clamp. *)
let clamp index ~max = Int.max 0 (Int.min max index)

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
  ~births
  ~flame
  (_ : _ Bonsai.Apply_action_context.t)
  (model : Model.t)
  action
  =
  let last = Replay.length replay - 1 in
  let clamp_step step = clamp step ~max:last in
  let clamp_frame index =
    clamp index ~max:(frame_count replay ~step:model.step - 1)
  in
  let toggle set x =
    match Set.mem set x with
    | true -> Set.remove set x
    | false -> Set.add set x
  in
  (* any move re-follows the innermost frame and drops the chosen box — at
     another step that address may name nothing, so blue goes back to
     following the walked structure. Folds persist; that is the point of
     keying them on structure shape rather than on a step's addresses. *)
  let move ~playing step =
    { model with
      step = clamp_step step
    ; selected_frame = None
    ; playing
    ; heap_selected = None
    ; land_seq = model.land_seq + 1
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
  | Frame_delta delta ->
    let selected = selected_frame replay model in
    { model with selected_frame = Some (clamp_frame (selected + delta)) }
  | Toggle_stack_fold call ->
    { model with stack_folds = toggle model.stack_folds call }
  | Toggle_stack_run head ->
    { model with stack_expanded = toggle model.stack_expanded head }
  | Toggle_source_fold fold ->
    { model with source_folds = toggle model.source_folds fold }
  | Toggle_stack_pane ->
    { model with stack_collapsed = not model.stack_collapsed }
  | Toggle_source_pane ->
    { model with source_collapsed = not model.source_collapsed }
  | Set_heap_view heap_view -> { model with heap_view }
  | Toggle_heap_view ->
    { model with heap_view = Action.Heap_view.toggle model.heap_view }
  | Toggle_heap_fold fold ->
    { model with heap_folds = toggle model.heap_folds fold }
  | Toggle_accordion -> { model with accordion = not model.accordion }
  | Toggle_address_order ->
    { model with sort_by_address = not model.sort_by_address }
  | Set_columns columns ->
    { model with
      heap_columns = Int.clamp_exn columns ~min:1 ~max:max_columns
    }
  (* [.] and [⌖]: back to a mark, alternating between the pinned box and the
     walked structure. The pin is NOT cleared — it is half of what this
     button navigates between, and clearing it would delete the destination
     on the way there. *)
  | Focus_latest ->
    { model with
      focus_seq = model.focus_seq + 1
    ; focus_target = Action.Focus_target.toggle model.focus_target
    }
  (* [/] always starts from empty: the old filter was shaped around whatever
     you were hunting last time *)
  | Begin_filter -> { model with typing_filter = true; heap_filter = "" }
  | Set_filter filter -> { model with heap_filter = filter }
  | Commit_filter -> { model with typing_filter = false }
  | Cancel_filter -> { model with typing_filter = false; heap_filter = "" }
  (* committing a box is exactly what clicking it does — jump to where it was
     allocated — and additionally pins it, so it stays blue and keeps showing
     at the new step *)
  | Select_heap_address address ->
    let stepped =
      match Map.find births address with
      | Some step -> move ~playing:false step
      | None -> model
    in
    { stepped with heap_selected = Some address }
  | Toggle_lod -> { model with lod = Action.Lod.toggle model.lod }
  | Cycle_edge_style ->
    { model with edge_style = Action.Edge_style.cycle model.edge_style }
  | Toggle_theme ->
    { model with theme_mode = Action.Theme_mode.toggle model.theme_mode }
  | Toggle_flame -> { model with flame_open = not model.flame_open }
  | Jump_flame path ->
    (match Flame_tree.find flame ~path with
     | None -> model
     | Some (node : Flame_tree.Node.t) -> move ~playing:false node.first_step)
  | Zoom_flame path -> { model with flame_zoom = path }
  | Reset_flame_zoom -> { model with flame_zoom = [] }
  | Set_hud { zoom_percent; tier } ->
    { model with hud_zoom_percent = zoom_percent; hud_tier = tier }
  | Quit -> model
;;

(* ── once-per-run data, ported from the TUI app ──────────────────────── *)

(* Every share this interface colors is RELATIVE TO THE RUN: divided by the
   busiest call rather than left as a fraction of the machine. Absolute
   shares are calibrated for a profile of one program's hot loop, and neither
   input here looks like that — a real profile of this exchange spends most
   of its samples inside Base and Bin_prot, and a call-frequency fallback
   spreads a long trace over hundreds of names. Both put every row on the
   ramp's cold stop, which is a picture of the denominator, not of the run.
   {!Theme.heat_thresholds} is spaced for this convention. *)
let normalize shares =
  let busiest =
    Array.fold shares ~init:0. ~f:(fun hottest share ->
      Float.max hottest (Option.value share ~default:0.))
  in
  match Float.( > ) busiest 0. with
  | false -> shares
  | true ->
    Array.map shares ~f:(Option.map ~f:(fun share -> share /. busiest))
;;

(* each call's share, joined once up front — the color its name renders in.
   With a perf profile the share is sampled compute; without one it falls
   back to the trace itself, so a replay with no [-perf-file] still reads
   hot-to-cold at a glance. *)
let heat_of_calls ~profile ~(calls : Call.t array) =
  match (profile : Heat_profile.t option) with
  | Some profile ->
    ( Array.map calls ~f:(fun (call : Call.t) ->
        match
          Heat_profile.share
            profile
            ~function_info:call.info.function_info
            ~location:call.info.location
        with
        | Some share -> Some share
        (* The instrumentation fires on bindings inside functions, so most
           callees arrive as expressions located mid-function: perf can name
           the enclosing function but the trace cannot, and a whole replay
           comes back neutral with a good profile loaded. Falling back to
           what the FILE cost keeps the reading true — coarser, not wrong. *)
        | None ->
          Heat_profile.file_share profile ~location:call.info.location)
      |> normalize
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
      |> normalize
    , `Calls )
;;

(* how much allocated at each step — the timeline's shading — as each step's
   share of the run's busiest step *)
let density_of_steps replay =
  let counts =
    Array.init (Replay.length replay) ~f:(fun step ->
      Set.length (Replay.step_exn replay ~step).new_addresses)
  in
  let busiest = Array.fold counts ~init:1 ~f:Int.max in
  Array.map counts ~f:(fun count ->
    Float.of_int count /. Float.of_int busiest)
;;

(* what each step's call put into the registry, by the name the heap lists it
   under — the [· m] / [· #826] tag on that call's row *)
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

(* where each address was first seen — what a click on a heap box jumps to *)
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

(* ── DOM effects ─────────────────────────────────────────────────────── *)

let scroll_into_center id =
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some element ->
    Js.Unsafe.meth_call
      element
      "scrollIntoView"
      [| Js.Unsafe.inject
           (Js.Unsafe.obj
              [| "block", Js.Unsafe.inject (Js.string "center")
               ; "inline", Js.Unsafe.inject (Js.string "nearest")
              |])
      |]
;;

(* the outline's walked-structure row rides along: it is the pane's landing
   place while the OUTLINE tab is up, and it is simply absent otherwise *)
let scroll_panes_effect =
  Effect.of_sync_fun (fun (stack_id, source_id) ->
    scroll_into_center stack_id;
    scroll_into_center source_id;
    scroll_into_center Outline_view.current_row_id)
;;

let close_window_effect =
  Effect.of_sync_fun (fun () -> Dom_html.window##close) ()
;;

(* The page behind the app — visible for a frame while loading, and behind
   rubber-band overscroll — follows the theme too. [color-scheme] is the
   other half: it is what makes the browser's OWN furniture, the pane
   scrollbars and the columns slider's track, pick this theme's side rather
   than the operating system's. *)
let page_theme_effect =
  Effect.of_sync_fun (fun (background, scheme) ->
    Dom_html.document##.body##.style##.background := Js.string background;
    Js.Unsafe.set
      Dom_html.document##.documentElement##.style
      (Js.string "colorScheme")
      (Js.string scheme))
;;

(* ── the component ───────────────────────────────────────────────────── *)

let component
  ?profile
  ~(replay : Replay.t)
  ~(sources : Source_model.Loaded.t Or_error.t String.Map.t)
  ~dump_name
  (local_ graph)
  =
  let births = birth_steps replay in
  let calls =
    Array.init (Replay.length replay) ~f:(fun step ->
      (Replay.step_exn replay ~step).call)
  in
  let heat, heat_source = heat_of_calls ~profile ~calls in
  let has_heat = Array.exists heat ~f:Option.is_some in
  let flame = Flame_tree.create ~calls ~profile in
  let density = density_of_steps replay in
  let segments = Timeline_model.segments ~density in
  let registered = registrations replay in
  let total = Replay.length replay in
  let model, inject_raw =
    Bonsai.state_machine
      ~sexp_of_model:Model.sexp_of_t
      ~sexp_of_action:Action.sexp_of_t
      ~equal:Model.equal
      ~default_model:Model.initial
      ~apply_action:(apply_action replay ~births ~flame)
      graph
  in
  (* [q] is a browser action, not a model change *)
  let inject =
    let%arr inject_raw in
    fun action ->
      match (action : Action.t) with
      | Quit -> Effect.Many [ inject_raw action; close_window_effect ]
      | (_ : Action.t) -> inject_raw action
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
  (* the scene: everything the canvas draws, rebuilt when the step or a heap
     mode changes *)
  let scene =
    let%arr { Model.step
            ; heap_folds
            ; heap_filter
            ; sort_by_address
            ; accordion
            ; _
            }
      =
      model
    in
    let { Replay.Step.structures; nodes; new_addresses; _ } =
      Replay.step_exn replay ~step
    in
    Heap_scene.build
      ~structures
      ~nodes
      ~new_addresses
      ~folds:heap_folds
      ~filter:heap_filter
      ~sort_by_address
      ~accordion
  in
  (* The pane's other reading of the same step: the outline's rows, built
     only while its tab is the one showing. They read the scene's inputs
     rather than the scene, so the two tabs claim and fold identically —
     {!Heap_outline}'s contract. *)
  let outline =
    let%arr { Model.step
            ; heap_view
            ; heap_folds
            ; heap_filter
            ; sort_by_address
            ; accordion
            ; _
            }
      =
      model
    in
    match heap_view with
    | Action.Heap_view.Diagram -> []
    | Outline ->
      let { Replay.Step.structures; nodes; new_addresses; _ } =
        Replay.step_exn replay ~step
      in
      Heap_outline.rows
        ~structures
        ~nodes
        ~new_addresses
        ~folds:heap_folds
        ~filter:heap_filter
        ~sort_by_address
        ~accordion
  in
  (* the four tier layouts, kept off the scene's own computation so that
     moving the columns slider re-places the same scene rather than
     rebuilding it — the canvas keeps its zoom anchor across the reflow *)
  let layouts =
    let%arr roots, (_ : Heap_scene.Stats.t) = scene
    and { Model.heap_columns; _ } = model in
    Heap_layout.all roots ~columns:heap_columns
  in
  let heap_canvas =
    let%arr roots, (_ : Heap_scene.Stats.t) = scene
    and layouts
    and { Model.step
        ; heap_columns
        ; land_seq
        ; focus_seq
        ; focus_target
        ; heap_selected
        ; heap_filter
        ; typing_filter
        ; lod
        ; edge_style
        ; theme_mode
        ; _
        }
      =
      model
    and inject in
    let current =
      List.find roots ~f:(fun (root : Heap_scene.Root.t) -> root.is_current)
    in
    let pulse_ids =
      List.concat_map roots ~f:(fun (root : Heap_scene.Root.t) ->
        Heap_scene.Node.fold root.node ~init:[] ~f:(fun acc node ->
          match node.is_new with
          | true -> Heap_layout.key_id node.key :: acc
          | false -> acc))
    in
    Heap_widget.view
      { Heap_widget.Input.roots
      ; layouts
      ; step
      ; land_seq
      ; focus_seq
      ; focus_target
      ; columns = heap_columns
      ; pulse_ids
      ; current_root_id =
          Option.map current ~f:(fun (root : Heap_scene.Root.t) ->
            Heap_layout.key_id root.node.key)
      ; current_structure_id =
          Option.map current ~f:(fun (root : Heap_scene.Root.t) ->
            root.structure_id)
      ; selected_address = heap_selected
      ; filter_active = not (String.is_empty heap_filter)
      ; lod
      ; edge_style
      ; typing_filter
      ; theme = Action.Theme_mode.palette theme_mode
      ; inject
      }
  in
  (* keep the selected call and the active source line centered as the replay
     moves — the TUI's landing, spelled scrollIntoView *)
  let scroll_key =
    let%arr { Model.step; selected_frame; _ } = model in
    let selected =
      clamp
        (Option.value selected_frame ~default:(frame_count replay ~step - 1))
        ~max:(frame_count replay ~step - 1)
    in
    let live = live_calls replay ~step in
    let selected_step =
      Option.value (List.nth live selected) ~default:step
    in
    let frame =
      Option.value
        (List.nth (Replay.step_exn replay ~step).frames selected)
        ~default:(Replay.step_exn replay ~step).call
    in
    ( [%string "stack-row-%{selected_step#Int}"]
    , [%string "src-line-%{Location.line_number frame.info.location#Int}"] )
  in
  Bonsai.Edge.on_change
    ~sexp_of_model:[%sexp_of: string * string]
    ~trigger:`After_display
    ~equal:[%equal: string * string]
    scroll_key
    ~callback:
      (Bonsai.return (fun key ->
         (* after this frame's DOM patch, so the rows exist *)
         scroll_panes_effect key))
    graph;
  let page_theme =
    let%arr { Model.theme_mode; _ } = model in
    let theme = Action.Theme_mode.palette theme_mode in
    theme.bg, theme.name
  in
  Bonsai.Edge.on_change
    ~sexp_of_model:[%sexp_of: string * string]
    ~equal:[%equal: string * string]
    page_theme
    ~callback:(Bonsai.return page_theme_effect)
    graph;
  let view =
    let%arr model
    and (_ : Heap_scene.Root.t list), stats = scene
    and heap_canvas
    and outline
    and inject in
    let { Model.step
        ; playing
        ; heap_view
        ; heap_selected
        ; accordion
        ; sort_by_address
        ; heap_columns
        ; focus_target
        ; heap_filter
        ; typing_filter
        ; stack_folds
        ; stack_expanded
        ; source_folds
        ; stack_collapsed
        ; source_collapsed
        ; lod
        ; edge_style
        ; theme_mode
        ; flame_open
        ; flame_zoom
        ; hud_zoom_percent
        ; hud_tier
        ; _
        }
      =
      model
    in
    let theme = Action.Theme_mode.palette theme_mode in
    let { Replay.Step.call; frames; _ } = Replay.step_exn replay ~step in
    let live = live_calls replay ~step in
    let selected = selected_frame replay model in
    let frame = Option.value (List.nth frames selected) ~default:call in
    let location = frame.info.location in
    let file_path = Location.file_path location in
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
    let timeline =
      Timeline_view.view
        ~theme
        ~segments
        ~step
        ~total
        ~playing
        ~accordion
        ~sort_by_address
        ~typing_filter
        ~lod
        ~theme_mode
        ~flame_open
        ~inject
    in
    let stack =
      Stack_view.view
        ~theme
        ~rows:
          (Stack_rows.rows
             ~calls
             ~heat
             ~live
             ~selected
             ~folds:stack_folds
             ~expanded:stack_expanded
             ~registered)
        ~total_calls:(Array.length calls)
        ~live_count:(List.length live)
        ~has_heat
        ~collapsed:stack_collapsed
        ~inject
    in
    let source_rows =
      match Map.find sources file_path with
      | Some (Ok loaded) ->
        let folds =
          Set.fold
            source_folds
            ~init:Int.Set.empty
            ~f:(fun acc { Action.Source_fold.file; line } ->
              match String.equal file file_path with
              | true -> Set.add acc line
              | false -> acc)
        in
        Ok
          ( Source_model.rows
              loaded
              ~folds
              ~active_line:(Location.line_number location)
              ~callsite_line
              ~char_range:(Location.char_range location)
          , Source_file.length loaded.file )
      | Some (Error error) -> Error error
      | None ->
        Or_error.error_s
          [%message "no source loaded for" ~file:(file_path : string)]
    in
    let source =
      let rows, line_count =
        match source_rows with
        | Ok (rows, line_count) -> Ok rows, line_count
        | Error error -> Error error, 0
      in
      Source_view.view
        ~theme
        ~source:rows
        ~file_label:(Filename.basename file_path)
        ~file:file_path
        ~line_count
        ~collapsed:source_collapsed
        ~inject
    in
    let flame_rows =
      match flame_open with
      | false -> []
      | true ->
        let width =
          Float.max
            300.
            (Float.of_int Dom_html.window##.innerWidth -. 436. -. 8.)
        in
        Flame_math.bars
          flame
          ~zoom:flame_zoom
          ~width
          ~live:(Flame_math.live_path flame ~frames)
    in
    let depth_count =
      1
      + List.fold
          flame_rows
          ~init:0
          ~f:(fun deepest (row : Flame_math.Row.t) ->
            Int.max deepest row.depth)
    in
    let flame_drawer =
      Flame_view.view
        ~theme
        ~tree:flame
        ~rows:flame_rows
        ~open_:flame_open
        ~zoomed:(not (List.is_empty flame_zoom))
        ~depth_count
        ~inject
    in
    let heap_meta =
      let counts =
        [%string
          "%{stats.Heap_scene.Stats.structures#Int} live · \
           %{stats.nodes#Int} nodes · %{stats.new_nodes#Int} new"]
      in
      let modes =
        List.filter_opt
          [ (match accordion with true -> Some "accordion" | false -> None)
          ; (match sort_by_address with
             | true -> Some "by address"
             | false -> None)
          ; (match
               Action.Edge_style.equal edge_style Action.Edge_style.Angled
             with
             | true -> None
             | false ->
               Some
                 (match edge_style with
                  | Action.Edge_style.Orthogonal -> "orthogonal edges"
                  | Curved -> "curved edges"
                  | Angled -> ""))
          ]
      in
      match modes with
      | [] -> counts
      | modes -> [%string "%{String.concat modes ~sep:\" · \"} · %{counts}"]
    in
    let filter_overlay =
      match typing_filter || not (String.is_empty heap_filter) with
      | false -> Vdom.Node.none
      | true ->
        let meta =
          match String.is_empty heap_filter with
          | true -> "esc"
          | false ->
            [%string
              "%{stats.Heap_scene.Stats.hits#Int} hit%{match stats.hits \
               with 1 -> \"\" | _ -> \"s\"}"]
        in
        let on_keydown (event : Dom_html.keyboardEvent Js.t) =
          let key = Js.Optdef.case event##.key (fun () -> "") Js.to_string in
          match key with
          | "Enter" -> inject Action.Commit_filter
          | "Escape" -> inject Action.Cancel_filter
          | (_ : string) -> Effect.Ignore
        in
        let on_input (_ : _) value = inject (Action.Set_filter value) in
        let autofocus =
          match typing_filter with
          | true -> Vdom.Attr.autofocus true
          | false -> Vdom.Attr.empty
        in
        {%html|
          <div %{Styles.filter_overlay theme}>
            <span %{Styles.filter_slash theme}>/</span>
            <input %{Styles.filter_input theme} %{autofocus}
                   placeholder="filter structures"
                   value=%{heap_filter}
                   on_keydown=%{on_keydown}
                   on_input=%{on_input} />
            <span %{Styles.filter_meta theme}>#{meta}</span>
          </div>
        |}
    in
    let showing_outline =
      match heap_view with
      | Action.Heap_view.Outline -> true
      | Diagram -> false
    in
    (* The two readings of the same scene. The canvas stays mounted under the
       outline rather than being swapped out for it: the widget owns the
       pane's keyboard, and an idle canvas costs nothing to leave behind an
       opaque panel. *)
    let outline_panel =
      match showing_outline with
      | false -> Vdom.Node.none
      | true ->
        Outline_view.view
          ~theme
          ~rows:outline
          ~selected:heap_selected
          ~inject
    in
    let tabs =
      let tab view =
        let selected = Action.Heap_view.equal view heap_view in
        let label = Action.Heap_view.display view in
        {%html|
          <span %{Styles.heap_tab theme ~selected}
                on_click=%{fun _ -> inject (Action.Set_heap_view view)}>#{label}</span>
        |}
      in
      {%html|
        <span %{Styles.heap_tabs}>
          %{tab Action.Heap_view.Diagram}
          %{tab Action.Heap_view.Outline}
        </span>
      |}
    in
    (* the zoom readout, the columns slider and the [⌖] mark are the canvas's
       own controls; none of them means anything over the outline, which is a
       list you scroll *)
    let canvas_only node =
      match showing_outline with true -> Vdom.Node.none | false -> node
    in
    let hud =
      let zoom = [%string "zoom %{hud_zoom_percent#Int}%"] in
      let tier = sprintf "detail %.1f / 3" hud_tier in
      let model_text = Action.Lod.display lod in
      {%html|
        <div %{Styles.hud theme}>
          <span %{Styles.hud_zoom theme}>#{zoom}</span>
          <span>#{tier}</span>
          <span %{Styles.hud_model theme}>#{model_text}</span>
        </div>
      |}
    in
    (* the corner control, sitting on the minimap: how many structures stand
       side by side. A real range input, so dragging, clicking the track and
       the arrow keys all work without any of them being written here — and
       the canvas's own key handler already steps aside for an [input]. *)
    let columns_slider =
      let on_input (_ : _) value =
        match Int.of_string_opt value with
        | Some columns -> inject (Action.Set_columns columns)
        | None -> Effect.Ignore
      in
      let range =
        Vdom.Attr.many
          [ Vdom.Attr.type_ "range"
          ; Vdom.Attr.create "min" "1"
          ; Vdom.Attr.create "max" (Int.to_string max_columns)
          ; Vdom.Attr.create "step" "1"
          ; Vdom.Attr.value (Int.to_string heap_columns)
          ; Styles.columns_slider theme
          ]
      in
      let label = [%string "%{heap_columns#Int} across"] in
      {%html|
        <div %{Styles.columns_panel theme}>
          <input %{range} on_input=%{on_input} />
          <span %{Styles.columns_label theme}>#{label}</span>
        </div>
      |}
    in
    let focus_chip =
      let label = Action.Focus_target.display focus_target in
      {%html|
        <span %{Styles.hint_chip theme.accent}
              on_click=%{fun _ -> inject Action.Focus_latest}>#{label}</span>
      |}
    in
    let session =
      Session_view.view
        ~theme
        ~dump_name
        ~heat:
          (match has_heat with false -> None | true -> Some heat_source)
    in
    {%html|
      <div %{Styles.root theme}>
        %{timeline}
        <div %{Styles.main theme}>
          <div %{Styles.left_column ~stack_collapsed ~source_collapsed}>
            %{stack}
            %{source}
          </div>
          <div %{Styles.heap_pane theme}>
            <div %{Styles.heap_header theme}>
              <span %{Styles.heap_title_group}>
                <span %{Styles.pane_title theme}>HEAP</span>
                %{tabs}
              </span>
              <span>
                %{canvas_only focus_chip}
                <span %{Styles.pane_meta theme}>#{heap_meta}</span>
              </span>
            </div>
            <div %{Styles.heap_body}>
              %{heap_canvas}
              %{canvas_only hud}
              %{canvas_only columns_slider}
              %{outline_panel}
              %{filter_overlay}
            </div>
            %{flame_drawer}
          </div>
        </div>
        %{session}
      </div>
    |}
  in
  view
;;

let run ?profile ~replay ~sources ~dump_name () =
  Bonsai_web.Start.start (fun (local_ graph) ->
    component ?profile ~replay ~sources ~dump_name graph)
;;

let run_error ~error =
  Bonsai_web.Start.start (fun (local_ (_ : Bonsai.graph)) ->
    Bonsai.return
      {%html|
        <div %{Styles.error_page Theme.dark}>
          <div>
            <div>jsip web debugger — cannot start</div>
            <div> </div>
            <div>#{Error.to_string_hum error}</div>
          </div>
        </div>
      |})
;;
