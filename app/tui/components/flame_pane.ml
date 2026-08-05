open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Path = struct
  type t = Flame_tree.Key.t list [@@deriving sexp_of, equal]
end

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, equal]
end

module Columns = struct
  type t =
    { children : int list
    ; pool : int
    ; pooled : int
    ; self : int
    }
  [@@deriving sexp_of, equal]

  let empty = { children = []; pool = 0; pooled = 0; self = 0 }
end

(* the indices of the [leftover] largest remainders, ties by draw order:
   Hamilton's bonus column *)
let bonus_indices remainders ~leftover =
  List.mapi remainders ~f:(fun index remainder -> index, remainder)
  |> List.sort ~compare:(fun (index, a) (other, b) ->
    match Int.descending a b with
    | 0 -> Int.compare index other
    | ordering -> ordering)
  |> (fun ranked -> List.take ranked (max 0 leftover))
  |> List.map ~f:fst
  |> Int.Set.of_list
;;

let columns ~width ~self ~children =
  let count = List.length children in
  let total = self + List.sum (module Int) children ~f:Fn.id in
  match width <= 0 || total <= 0 with
  | true -> Columns.empty
  | false ->
    (* capacity first: how many children can hold a column of their own. The
       [+N] is floored wider than a bar, because a one-column pool cannot say
       how many children it stands for and so reads as one more bar — except
       on a row too narrow for even that, where a single column saying "more"
       still beats silence. *)
    let pool_floor = match width >= 4 with true -> 3 | false -> 1 in
    let drawn_count =
      match count <= width with
      | true -> count
      | false -> max 0 (width - pool_floor)
    in
    let pooled = count - drawn_count in
    let kept = List.take children drawn_count in
    let bar_floors = List.map kept ~f:(fun (_ : int) -> 1) in
    let drawn, floors =
      match pooled with
      | 0 -> kept, bar_floors
      | (_ : int) ->
        ( kept
          @ [ List.sum (module Int) (List.drop children drawn_count) ~f:Fn.id
            ]
        , bar_floors @ [ pool_floor ] )
    in
    (* the floors are spent up front; Hamilton divides what is left over the
       true weights, with the parent's own calls last in draw order and no
       floor of its own — an unlabelled gap that rounds away hides nothing *)
    let remaining = width - List.sum (module Int) floors ~f:Fn.id in
    let weights = drawn @ [ self ] in
    let quotas = List.map weights ~f:(fun weight -> weight * remaining) in
    let base = List.map quotas ~f:(fun quota -> quota / total) in
    let bonus =
      bonus_indices
        (List.map quotas ~f:(fun quota -> quota % total))
        ~leftover:(remaining - List.sum (module Int) base ~f:Fn.id)
    in
    let allocated =
      List.mapi base ~f:(fun index cells ->
        match Set.mem bonus index with true -> cells + 1 | false -> cells)
    in
    let bars =
      List.map2_exn
        (List.take allocated (List.length floors))
        floors
        ~f:( + )
    in
    let self = List.last_exn allocated in
    (match pooled with
     | 0 -> { Columns.children = bars; pool = 0; pooled = 0; self }
     | (_ : int) ->
       { Columns.children = List.drop_last_exn bars
       ; pool = List.last_exn bars
       ; pooled
       ; self
       })
;;

let live_path tree ~frames =
  Flame_tree.path tree ~frames
  |> List.map ~f:(fun (node : Flame_tree.Node.t) -> node.key)
;;

(* what one stretch of a row is: a bar, the [+N] standing in for the children
   that could not fit, or the parent's own calls *)
module Segment = struct
  type t =
    | Bar of
        { path : Path.t
        ; node : Flame_tree.Node.t
        }
    | Pool of int
    | Self
end

module Placed = struct
  type t =
    { segment : Segment.t
    ; x : int
    ; width : int
    }

  let path t =
    match t.segment with
    | Bar { path; node = (_ : Flame_tree.Node.t) } -> Some path
    | Pool (_ : int) | Self -> None
  ;;
end

let place ~x sized =
  List.folding_map sized ~init:x ~f:(fun x (segment, width) ->
    x + width, { Placed.segment; x; width })
;;

(* one bar's children and its own calls, laid out across the columns the bar
   itself occupies — children first, [+N] behind them, the self run last so
   that a subtree always starts at its caller's left edge *)
let spread ~x ~width ~self ~children ~path_of =
  let weights =
    List.map children ~f:(fun (child : Flame_tree.Node.t) -> child.inclusive)
  in
  let cells = columns ~width ~self ~children:weights in
  let bars =
    List.map2_exn
      (List.take children (List.length cells.children))
      cells.children
      ~f:(fun child width ->
        Segment.Bar { path = path_of child; node = child }, width)
  in
  let pool =
    match cells.pooled with
    | 0 -> []
    | pooled -> [ Segment.Pool pooled, cells.pool ]
  in
  let own =
    match cells.self with 0 -> [] | width -> [ Segment.Self, width ]
  in
  place ~x (bars @ pool @ own)
;;

let row_below row =
  List.concat_map row ~f:(fun (placed : Placed.t) ->
    match placed.segment with
    | Pool _ | Self -> []
    | Bar { path; node } ->
      (* a leaf ends: its whole bar is already its own, so nothing goes under
         it *)
      (match node.children with
       | [] -> []
       | children ->
         spread
           ~x:placed.x
           ~width:placed.width
           ~self:node.calls
           ~children
           ~path_of:(fun (child : Flame_tree.Node.t) -> path @ [ child.key ])))
;;

let rows (tree : Flame_tree.t) ~width ~zoom =
  let top =
    match Flame_tree.find tree ~path:zoom with
    | Some node ->
      [ { Placed.segment = Bar { path = zoom; node }; x = 0; width } ]
    | None ->
      (* not zoomed, or a path that no longer resolves: the roots share the
         top row, measured against the whole run *)
      spread
        ~x:0
        ~width
        ~self:0
        ~children:tree.roots
        ~path_of:(fun (root : Flame_tree.Node.t) -> [ root.key ])
  in
  let rec build row acc =
    match row with
    | [] -> List.rev acc
    | (_ : Placed.t list) -> build (row_below row) (row :: acc)
  in
  build top []
;;

let body_height ~height = max 0 (height - Panel.header_height)

let index_of_path rows path =
  List.findi rows ~f:(fun (_ : int) row ->
    List.exists row ~f:(fun (placed : Placed.t) ->
      match Placed.path placed with
      | Some other -> Path.equal other path
      | None -> false))
  |> Option.map ~f:fst
;;

(* the deepest row the live stack reaches inside the zoom — where the eye
   goes when nothing has been aimed at yet *)
let live_row rows ~live =
  List.filter_mapi rows ~f:(fun index row ->
    match
      List.exists row ~f:(fun (placed : Placed.t) ->
        match Placed.path placed with
        | Some path ->
          List.is_prefix live ~prefix:path ~equal:Flame_tree.Key.equal
        | None -> false)
    with
    | true -> Some index
    | false -> None)
  |> List.last
;;

(* one clamp, called by [view], [bar_at] and [move_cursor] alike, so the
   picture and the click math cannot drift *)
let offset rows ~height ~cursor ~live ~depth_scroll =
  let depth = List.length rows in
  let furthest = max 0 (depth - height) in
  let clamp value = Int.clamp_exn value ~min:0 ~max:furthest in
  let target =
    match cursor with
    | Some cursor -> index_of_path rows cursor
    | None -> live_row rows ~live
  in
  match target with
  | None -> clamp depth_scroll
  | Some target ->
    let start = clamp depth_scroll in
    (match target < start, target >= start + height with
     | true, _ -> clamp target
     | false, true -> clamp (target - height + 1)
     | false, false -> start)
;;

(* the screen, one entry per body row, [None] where nothing is drawn.

   The root sits on the BOTTOM row and the stack grows upward from it — that
   is what makes it read as flames — so the visible levels are laid out
   deepest-first and the spare rows go above them. Every consumer goes
   through this, which is why the picture and the click math cannot disagree
   about which level a screen row shows. *)
let screen_rows rows ~height ~cursor ~live ~depth_scroll =
  let start = offset rows ~height ~cursor ~live ~depth_scroll in
  let shown = List.take (List.drop rows start) height in
  ( start
  , List.length shown
  , List.init (max 0 (height - List.length shown)) ~f:(fun (_ : int) -> None)
    @ List.rev_map shown ~f:Option.some )
;;

(* shrink until the label's DISPLAY width fits: cutting a multi-byte glyph in
   half would otherwise widen the cell it lands in *)
let truncate label ~columns =
  let rec shrink length =
    match length <= 0 with
    | true -> ""
    | false ->
      let candidate = String.prefix label length in
      (match View.width (View.text candidate) <= columns with
       | true -> candidate
       | false -> shrink (length - 1))
  in
  match columns <= 0 with
  | true -> ""
  | false -> shrink (String.length label)
;;

(* a box is FILLED, so its text is the label padded out to the whole box —
   the color does the drawing and the label rides on it *)
let padded label ~columns =
  let length = View.width (View.text label) in
  match columns >= length with
  | true -> [%string "%{label}%{String.make (columns - length) ' '}"]
  | false -> truncate label ~columns
;;

(* the label is inset a column when the box has room for it. Below four
   columns the ellipsis costs more than it says — two letters of a name
   narrow the guess, [m⋯] does not. *)
let label_text label ~columns =
  let length = View.width (View.text label) in
  match columns >= length + 2, columns >= length, columns >= 4 with
  | true, _, _ -> padded [%string " %{label}"] ~columns
  | false, true, _ -> padded label ~columns
  | false, false, true ->
    let shown = truncate label ~columns:(columns - 1) in
    [%string "%{shown}⋯"]
  | false, false, false -> truncate label ~columns
;;

(* A filled box, the way a flame graph draws one: the color IS the rectangle,
   and the label is ink on it. [is_last] gives up the gutter at the row's
   right end — a trailing gap there would read as the row falling short of
   the pane. The gutter belongs to the box it follows, so a click on it lands
   where the eye says it should. *)
let box_view ~label ~width ~fill ~ink ~bold ~lit ~is_last =
  let gutter = match is_last || width <= 2 with true -> 0 | false -> 1 in
  let mark, room =
    match lit with
    | true -> 1, width - gutter - 1
    | false -> 0, width - gutter
  in
  View.hcat
    [ (match mark with
       | 0 -> View.text ""
       (* the lit path's edge marker sits INSIDE the box, in the ink color,
          so it cannot be mistaken for a box of its own *)
       | (_ : int) -> View.text ~attrs:[ Theme.fg ink; Attr.bg fill ] "▏")
    ; View.text
        ~attrs:
          ([ Theme.fg ink; Attr.bg fill ]
           @ match bold with true -> [ Attr.bold ] | false -> [])
        (label_text label ~columns:(max 0 room))
    ; View.text (String.make gutter ' ')
    ]
;;

(* What the colors mean. With a profile loaded they are sampled compute; with
   none they fall back to the trace itself — a box's share of the run's
   events — so the graph still grades hot-to-cold instead of going flat gray,
   exactly the fallback the stack pane's heat already makes. The meta line
   names which of the two, so the same colors never quietly change meaning. *)
let heat_source (tree : Flame_tree.t) =
  match
    Map.exists tree.functions ~f:(fun (metrics : Flame_tree.Metrics.t) ->
      Option.is_some metrics.share)
  with
  | true -> `Compute
  | false -> `Calls
;;

let box_share (tree : Flame_tree.t) (node : Flame_tree.Node.t) ~source =
  match source with
  | `Compute -> Flame_tree.prorated_share tree node
  | `Calls ->
    (match tree.total_events with
     | 0 -> None
     | total -> Some (Float.of_int node.inclusive /. Float.of_int total))
;;

let segment_view tree (placed : Placed.t) ~lit ~cursor ~is_last ~source =
  match placed.segment with
  | Self ->
    (* the parent's own calls: no box, because no frame ran there. In a flame
       graph this is simply the exposed top edge of the box below. *)
    View.text (String.make placed.width ' ')
  | Pool count ->
    (* it stands for work but measures none of it, so it is drawn as chrome
       rather than as a box: no fill, no place on the ramp *)
    View.text
      ~attrs:(Theme.fg' Theme.muted)
      (padded [%string "+%{count#Int}"] ~columns:placed.width)
  | Bar { path; node } ->
    let is_cursor =
      match cursor with
      | Some cursor -> Path.equal cursor path
      | None -> false
    in
    let fill, ink =
      match box_share tree node ~source with
      | Some share -> Theme.flame ~share, Theme.flame_label
      | None -> Theme.flame_neutral, Theme.flame_label_neutral
    in
    (* the keyboard cursor takes the whole box, because on a filled box a
       background wash is the only cue there is *)
    let fill, ink =
      match is_cursor with
      | true -> Theme.cursor, Theme.flame_label
      | false -> fill, ink
    in
    box_view
      ~label:(Flame_tree.Key.display node.key)
      ~width:placed.width
      ~fill
      ~ink
      ~bold:(Path.equal lit path)
      ~lit:(List.is_prefix lit ~prefix:path ~equal:Flame_tree.Key.equal)
      ~is_last
;;

let row_view tree row ~width ~lit ~cursor ~source =
  let last = List.length row - 1 in
  Panel.row
    (View.hcat
       (List.mapi row ~f:(fun index placed ->
          segment_view
            tree
            placed
            ~lit
            ~cursor
            ~source
            ~is_last:(index = last))))
    ~width
;;

let meta (tree : Flame_tree.t) ~zoom ~start ~shown ~depth =
  let zoomed =
    match List.last zoom with
    | None -> []
    | Some key -> [ [%string "⌖ %{Flame_tree.Key.display key}"] ]
  in
  (* the same disclosure the session bar's ramp legend makes: the colors are
     either sampled compute or the trace's own call volume, and which one it
     is has to be on screen or they silently change meaning *)
  let colors =
    match heat_source tree with
    | `Compute -> "color = compute"
    | `Calls -> "color = calls"
  in
  (* a shut drawer draws no rows at all, so there is no range to name — and
     [start + shown - 1] would name a row before the first one *)
  let depths =
    match shown <= 0 || shown >= depth with
    | true -> []
    | false ->
      [ [%string
          "depth %{start#Int}-%{start + shown - 1#Int} of %{depth#Int}"]
      ]
  in
  let events = [%string "%{tree.total_events#Int} events"] in
  (* [Panel.view] crops the meta from the right, so what the drawer is doing
     right now goes ahead of what it always means *)
  zoomed @ depths @ [ events; "width = calls"; colors ]
  |> String.concat ~sep:" · "
;;

let view ~width ~height ~open_ ~tree ~live ~zoom ~cursor ~depth_scroll =
  let inner_width = Panel.inner_width ~width in
  let rows = rows tree ~width:inner_width ~zoom in
  let start, shown, screen =
    screen_rows
      rows
      ~height:(body_height ~height)
      ~cursor
      ~live
      ~depth_scroll
  in
  let source = heat_source tree in
  (* the drawer's own affordance, in the glyph vocabulary the stack and heap
     panes already fold with: [▾] open, [▸] shut *)
  let title = match open_ with true -> "▾ flame" | false -> "▸ flame" in
  Panel.view
    ~title
    ~meta:(meta tree ~zoom ~start ~shown ~depth:(List.length rows))
    ~width
    ~height
    (View.vcat
       (List.map screen ~f:(fun row ->
          match row with
          | None -> Panel.row (View.text "") ~width:inner_width
          | Some row ->
            row_view tree row ~width:inner_width ~lit:live ~cursor ~source)))
;;

let bar_at ~width ~height ~tree ~live ~zoom ~cursor ~depth_scroll ~x ~row =
  let rows = rows tree ~width:(Panel.inner_width ~width) ~zoom in
  let (_ : int), (_ : int), screen =
    screen_rows
      rows
      ~height:(body_height ~height)
      ~cursor
      ~live
      ~depth_scroll
  in
  match Option.join (List.nth screen row) with
  | None -> None
  | Some row ->
    List.find_map row ~f:(fun (placed : Placed.t) ->
      match x >= placed.x && x < placed.x + placed.width with
      | true -> Placed.path placed
      | false -> None)
;;

let bars_in row =
  List.filter_map row ~f:(fun (placed : Placed.t) -> Placed.path placed)
;;

let move_cursor ~width ~tree ~zoom ~cursor ~live ~direction =
  let rows = rows tree ~width:(Panel.inner_width ~width) ~zoom in
  let start =
    match cursor with
    | Some cursor -> Some cursor
    | None ->
      (* nothing aimed at yet: begin where the replay is standing, or at the
         widest bar of the top row *)
      Option.first_some
        (Option.bind (live_row rows ~live) ~f:(fun index ->
           List.nth rows index
           |> Option.bind ~f:(fun row ->
             List.find (bars_in row) ~f:(fun path ->
               List.is_prefix live ~prefix:path ~equal:Flame_tree.Key.equal))))
        (Option.bind (List.hd rows) ~f:(fun row -> List.hd (bars_in row)))
  in
  match cursor, start with
  | None, (Some _ | None) -> start
  | Some (_ : Path.t), None -> None
  | Some (_ : Path.t), Some here ->
    let row_index = index_of_path rows here in
    (match row_index with
     | None -> None
     | Some row_index ->
       let sideways step =
         match List.nth rows row_index with
         | None -> None
         | Some row ->
           let bars = bars_in row in
           (match
              List.findi bars ~f:(fun (_ : int) path -> Path.equal path here)
            with
            | None -> None
            | Some (index, (_ : Path.t)) -> List.nth bars (index + step))
       in
       (* the directions are SPATIAL, because the flames rise: the callees
          are stacked above their caller, so [Up] goes deeper into the run
          and [Down] returns toward the root. (The heap pane's [w] climbs to
          a parent for the same reason — its tree is drawn root-first, this
          one root-last.) *)
       let toward =
         match (direction : Direction.t) with
         | Up -> `Deeper
         | Down -> `Shallower
         | Left -> `Left
         | Right -> `Right
       in
       (match toward with
        | `Shallower ->
          (match row_index with
           | 0 -> None
           | (_ : int) -> List.drop_last here)
        | `Deeper ->
          (* siblings are in name order, so the widest callee is not the
             first one; it is worth aiming at, so find it by width *)
          (match List.nth rows (row_index + 1) with
           | None -> None
           | Some row ->
             List.filter row ~f:(fun (placed : Placed.t) ->
               match Placed.path placed with
               | None -> false
               | Some path ->
                 List.is_prefix path ~prefix:here ~equal:Flame_tree.Key.equal
                 && List.length path = List.length here + 1)
             |> List.max_elt ~compare:(fun (a : Placed.t) b ->
               match Int.compare a.width b.width with
               | 0 -> Int.compare b.x a.x
               | ordering -> ordering)
             |> Option.bind ~f:Placed.path)
        | `Left -> sideways (-1)
        | `Right -> sideways 1))
;;
