open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Loaded = struct
  type t =
    { file : Source_file.t
    ; spans : (Syntax.Token.t * string) list Array.t
    ; regions : (int * int) list
    ; regions_by_start : int Int.Map.t
    (** [regions] keyed by its first line — the pane asks "does a region
        start here?" once per line of the file, per frame *)
    }

  (* a foldable region: a top-level definition — a line starting at column 0
     — and everything under it until the next one. Only regions with
     something to hide (two lines or more) fold. *)
  let regions file =
    let length = Source_file.length file in
    let is_start number =
      match Source_file.line file ~number with
      | None | Some "" -> false
      | Some line -> not (Char.is_whitespace line.[0])
    in
    let starts =
      List.filter (List.init length ~f:(fun i -> i + 1)) ~f:is_start
    in
    let rec pair starts =
      match starts with
      | [] -> []
      | [ last ] -> [ last, length ]
      | start :: (next :: _ as rest) -> (start, next - 1) :: pair rest
    in
    pair starts |> List.filter ~f:(fun (start, stop) -> stop > start)
  ;;

  let of_source_file file =
    let regions = regions file in
    { file
    ; spans = Syntax.file file
    ; regions
    ; regions_by_start = Int.Map.of_alist_exn regions
    }
  ;;
end

let token_attrs (token : Syntax.Token.t) =
  match token with
  | Keyword -> Theme.fg' Theme.keyword
  | Uident -> Theme.fg' Theme.ident
  | String -> Theme.fg' Theme.string_lit
  | Number -> Theme.fg' Theme.number
  | Comment -> [ Theme.fg Theme.faint; Attr.italic ]
  | Operator -> Theme.fg' Theme.muted
  | Plain -> Theme.fg' Theme.text
;;

(* split the line's spans so [char_range] can carry an underline *)
let underline_range spans ~char_range:(range_start, range_stop) =
  let column = ref 0 in
  List.concat_map spans ~f:(fun (token, text) ->
    let start = !column in
    let stop = start + String.length text in
    column := stop;
    let slice a b = String.sub text ~pos:(a - start) ~len:(b - a) in
    let plain a b =
      match b > a with true -> [ token, slice a b, false ] | false -> []
    in
    let marked a b =
      match b > a with true -> [ token, slice a b, true ] | false -> []
    in
    let overlap_start = Int.max start range_start in
    let overlap_stop = Int.min stop range_stop in
    match overlap_stop > overlap_start with
    | false -> [ token, text, false ]
    | true ->
      plain start overlap_start
      @ marked overlap_start overlap_stop
      @ plain overlap_stop stop)
;;

let gutter_width = 5

(* one file line as one or more visual lines: the gutter number, the fold
   glyph, and the callsite marker sit on the first; the active wash and bar
   span all of them, and long lines wrap instead of cropping *)
let code_lines
  ~width
  (loaded : Loaded.t)
  ~number
  ~active
  ~callsite
  ~region
  ~char_range
  =
  let bg =
    match active with true -> Some Theme.highlight_bg | false -> None
  in
  let spans = loaded.spans.(number - 1) in
  let spans =
    match active with
    | true -> underline_range spans ~char_range
    | false -> List.map spans ~f:(fun (token, text) -> token, text, false)
  in
  let pairs =
    List.map spans ~f:(fun (token, text, underlined) ->
      (token, underlined), text)
  in
  let text_width = max 8 (width - gutter_width - 2) in
  (* continuations tuck under the line's own indentation *)
  let continuation_indent =
    let raw =
      Option.value (Source_file.line loaded.file ~number) ~default:""
    in
    Int.min
      (String.length (String.take_while raw ~f:(Char.equal ' ')) + 2)
      (text_width / 2)
  in
  let wrapped =
    Wrap.spans
      pairs
      ~first_width:text_width
      ~width:(max 8 (text_width - continuation_indent))
  in
  List.mapi wrapped ~f:(fun line_index line_spans ->
    let marker =
      match active, callsite && line_index = 0 with
      | true, _ -> View.text ~attrs:(Theme.fg' Theme.highlight) "▎"
      | false, true -> View.text ~attrs:(Theme.fg' Theme.highlight) "▸"
      | false, false -> View.text " "
    in
    let fold_glyph =
      match line_index, region with
      | 0, Some (_ : int * int) ->
        View.text ~attrs:(Theme.fg' Theme.secondary) "▾"
      | _ -> View.text " "
    in
    let gutter =
      match line_index with
      | 0 ->
        View.text
          ~attrs:(Theme.fg' Theme.ghost)
          (Printf.sprintf "%*d " (gutter_width - 2) number)
      | _ ->
        View.text (String.make (gutter_width - 1 + continuation_indent) ' ')
    in
    let code =
      List.map line_spans ~f:(fun ((token, underlined), text) ->
        let attrs = token_attrs token in
        let attrs =
          match underlined with
          | true -> Attr.underline :: attrs
          | false -> attrs
        in
        View.text ~attrs text)
    in
    Panel.row ?bg (View.hcat (marker :: fold_glyph :: gutter :: code)) ~width)
;;

(* a folded region as its single stand-in row: the first line, then how much
   is tucked away; washed when it hides the active line *)
let folded_line ~width (loaded : Loaded.t) ~start ~stop ~hides_active =
  let bg =
    match hides_active with true -> Some Theme.highlight_bg | false -> None
  in
  let marker =
    match hides_active with
    | true -> View.text ~attrs:(Theme.fg' Theme.highlight) "▎"
    | false -> View.text " "
  in
  let fold_glyph = View.text ~attrs:(Theme.fg' Theme.secondary) "▸" in
  let gutter =
    View.text
      ~attrs:(Theme.fg' Theme.ghost)
      (Printf.sprintf "%*d " (gutter_width - 2) start)
  in
  let code =
    List.map
      loaded.spans.(start - 1)
      ~f:(fun (token, text) -> View.text ~attrs:(token_attrs token) text)
  in
  let suffix =
    View.text
      ~attrs:[ Theme.fg Theme.muted; Attr.italic ]
      [%string " ⋯ %{stop - start#Int} lines"]
  in
  Panel.row
    ?bg
    (View.hcat ((marker :: fold_glyph :: gutter :: code) @ [ suffix ]))
    ~width
;;

(* every visible visual line, tagged with the file lines it accounts for (a
   folded row accounts for its whole region) and whether its fold column
   toggles a region *)
module Visual = struct
  type t =
    { line : int
    ; last : int
    ; toggle : int option
    ; view : View.t
    }
end

let build_visual_lines
  ~width
  (loaded : Loaded.t)
  ~folds
  ~active_line
  ~callsite_line
  ~char_range
  =
  let region_at number =
    Map.find loaded.regions_by_start number
    |> Option.map ~f:(fun stop -> number, stop)
  in
  let rec walk number acc =
    match number > Source_file.length loaded.file with
    | true -> List.rev acc
    | false ->
      (match region_at number with
       | Some (start, stop) when Set.mem folds start ->
         let hides_active = active_line > start && active_line <= stop in
         let view = folded_line ~width loaded ~start ~stop ~hides_active in
         walk
           (stop + 1)
           ({ Visual.line = number; last = stop; toggle = Some start; view }
            :: acc)
       | region ->
         let views =
           code_lines
             ~width
             loaded
             ~number
             ~active:(number = active_line)
             ~callsite:
               (match callsite_line with
                | Some line -> line = number
                | None -> false)
             ~region
             ~char_range
         in
         let toggle = Option.map region ~f:fst in
         let acc =
           List.rev_mapi views ~f:(fun i view ->
             { Visual.line = number
             ; last = number
             ; toggle = (match i with 0 -> toggle | _ -> None)
             ; view
             })
           @ acc
         in
         walk (number + 1) acc)
  in
  walk 1 []
;;

(* One frame's visual lines, remembered under everything they read — the pane
   re-renders on every heap scroll tick, and re-wrapping and re-folding the
   whole file each time is wasted work while nothing about the file view
   changed. *)
module Visual_key = struct
  type t =
    { width : int
    ; loaded : Loaded.t
    ; folds : Int.Set.t
    ; active_line : int
    ; callsite_line : int option
    ; char_range : int * int
    }

  let equal a b =
    a.width = b.width
    && phys_equal a.loaded b.loaded
    && Set.equal a.folds b.folds
    && a.active_line = b.active_line
    && [%equal: int option] a.callsite_line b.callsite_line
    && [%equal: int * int] a.char_range b.char_range
  ;;
end

let visual_cache : (Visual_key.t * Visual.t list) option ref = ref None

let visual_lines ~width loaded ~folds ~active_line ~callsite_line ~char_range
  =
  let key =
    { Visual_key.width
    ; loaded
    ; folds
    ; active_line
    ; callsite_line
    ; char_range
    }
  in
  match !visual_cache with
  | Some (cached, result) when Visual_key.equal cached key -> result
  | Some _ | None ->
    let result =
      build_visual_lines
        ~width
        loaded
        ~folds
        ~active_line
        ~callsite_line
        ~char_range
    in
    visual_cache := Some (key, result);
    result
;;

let scroll_offset visual ~height ~active_line =
  (* the row that accounts for the active line — its own first wrap row, or
     the folded row hiding it *)
  let target =
    List.findi
      visual
      ~f:(fun (_ : int) { Visual.last; line = _; toggle = _; view = _ } ->
        last >= active_line)
    |> Option.value_map ~default:0 ~f:fst
  in
  Int.min
    (Int.max 0 (target - (height / 2)))
    (Int.max 0 (List.length visual - height))
;;

let body
  ~width
  ~height
  ~source
  ~folds
  ~active_line
  ~callsite_line
  ~char_range
  =
  match (source : Loaded.t Or_error.t) with
  | Error error ->
    (* the placeholder has to be readable in a narrow pane: the message says
       where we looked and how to fix it, so it wraps instead of cropping *)
    let wrapped =
      Wrap.spans
        [ (), Error.to_string_hum error ]
        ~width:(Int.max 8 (width - 2))
    in
    View.pad
      ~l:1
      ~t:1
      (View.vcat
         (List.map wrapped ~f:(fun spans ->
            View.text
              ~attrs:[ Theme.fg Theme.faint; Attr.italic ]
              (String.concat
                 (List.map spans ~f:(fun ((() : unit), text) -> text))))))
  | Ok loaded ->
    let visual =
      visual_lines
        ~width
        loaded
        ~folds
        ~active_line
        ~callsite_line
        ~char_range
    in
    let offset = scroll_offset visual ~height ~active_line in
    View.vcat
      (List.drop visual offset
       |> List.map ~f:(fun { Visual.view; line = _; last = _; toggle = _ } ->
         view))
;;

let view
  ~width
  ~height
  ~file_label
  ~source
  ~folds
  ~active_line
  ~callsite_line
  ~char_range
  ~collapsed
  =
  let lines_label =
    match (source : Loaded.t Or_error.t) with
    | Ok loaded -> [%string "%{Source_file.length loaded.file#Int} lines"]
    | Error _ -> "missing"
  in
  (* collapsed, the pane is its title row — the [▸] is the way back in *)
  let title =
    match collapsed with true -> "▸ source" | false -> "▾ source"
  in
  let meta = [%string "%{file_label} · %{lines_label}"] in
  match collapsed with
  | true -> Panel.view ~title ~meta ~width ~height View.none
  | false ->
    Panel.view
      ~title
      ~meta
      ~width
      ~height
      (body
         ~width:(Panel.inner_width ~width)
         ~height:(height - Panel.header_height)
         ~source
         ~folds
         ~active_line
         ~callsite_line
         ~char_range)
;;

let toggle_at
  ~width
  ~height
  ~source
  ~folds
  ~active_line
  ~callsite_line
  ~char_range
  ~x
  ~y
  =
  match (source : Loaded.t Or_error.t) with
  | Error (_ : Error.t) -> None
  | Ok loaded ->
    let width = Panel.inner_width ~width in
    let height = height - Panel.header_height in
    let visual =
      visual_lines
        ~width
        loaded
        ~folds
        ~active_line
        ~callsite_line
        ~char_range
    in
    let offset = scroll_offset visual ~height ~active_line in
    (match x with
     | 1 ->
       List.nth visual (y + offset)
       |> Option.bind
            ~f:(fun { Visual.toggle; line = _; last = _; view = _ } ->
              toggle)
     | _ -> None)
;;
