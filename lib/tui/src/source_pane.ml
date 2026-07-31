open! Core
open Jsip_types
module Attr = Bonsai_term.Attr
module View = Bonsai_term.View

module Loaded = struct
  type t =
    { file : Source_file.t
    ; spans : (Syntax.Token.t * string) list Array.t
    }

  let of_source_file file = { file; spans = Syntax.file file }
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

(* one file line as one or more visual lines: the gutter number and the
   callsite marker sit on the first, the active wash and bar span all of
   them, and long lines wrap instead of cropping *)
let code_lines
  ~width
  (loaded : Loaded.t)
  ~number
  ~active
  ~callsite
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
  let text_width = max 8 (width - gutter_width - 1) in
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
    let line =
      Panel.fit (View.hcat (marker :: gutter :: code)) ~width ~height:1
    in
    match bg with
    | Some bg -> View.with_colors' ~fill_backdrop:true ~bg line
    | None -> line)
;;

let body ~width ~height ~source ~active_line ~callsite_line ~char_range =
  match (source : Loaded.t Or_error.t) with
  | Error error ->
    View.pad
      ~l:1
      ~t:1
      (View.text
         ~attrs:[ Theme.fg Theme.faint; Attr.italic ]
         (Error.to_string_hum error))
  | Ok loaded ->
    let visual_lines =
      List.init (Source_file.length loaded.file) ~f:(fun index ->
        let number = index + 1 in
        code_lines
          ~width
          loaded
          ~number
          ~active:(number = active_line)
          ~callsite:
            (match callsite_line with
             | Some line -> line = number
             | None -> false)
          ~char_range
        |> List.map ~f:(fun view -> number, view))
      |> List.concat
    in
    (* a location past EOF (a fixture newer than its source, say) still lands
       the view near the end instead of snapping to the top *)
    let target_line = Int.min active_line (Source_file.length loaded.file) in
    let active_start =
      List.findi visual_lines ~f:(fun (_ : int) (number, (_ : View.t)) ->
        number = target_line)
      |> Option.value_map ~default:0 ~f:fst
    in
    let offset =
      Int.min
        (Int.max 0 (active_start - (height / 2)))
        (Int.max 0 (List.length visual_lines - height))
    in
    View.vcat
      (List.drop visual_lines offset
       |> List.map ~f:(fun ((_ : int), view) -> view))
;;

let view
  ~width
  ~height
  ~file_label
  ~source
  ~active_line
  ~callsite_line
  ~char_range
  =
  let lines_label =
    match (source : Loaded.t Or_error.t) with
    | Ok loaded -> [%string "%{Source_file.length loaded.file#Int} lines"]
    | Error _ -> "missing"
  in
  Panel.view
    ~strong:true
    ~title:"source"
    ~meta:[%string "%{file_label} · %{lines_label}"]
    ~width
    ~height
    (body
       ~width:(Panel.inner_width ~width)
       ~height:(height - 2)
       ~source
       ~active_line
       ~callsite_line
       ~char_range)
;;
