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

let code_line
  ~width
  (loaded : Loaded.t)
  ~number
  ~active
  ~callsite
  ~char_range
  =
  let marker, marker_attrs =
    match active, callsite with
    | true, _ -> "▎", Theme.fg' Theme.accent
    | false, true -> "▸", Theme.fg' Theme.accent_deep
    | false, false -> " ", []
  in
  let number_view =
    View.text
      ~attrs:(Theme.fg' Theme.ghost)
      (Printf.sprintf "%*d " (gutter_width - 2) number)
  in
  let spans = loaded.spans.(number - 1) in
  let spans =
    match active with
    | true -> underline_range spans ~char_range
    | false -> List.map spans ~f:(fun (token, text) -> token, text, false)
  in
  let code =
    List.map spans ~f:(fun (token, text, underlined) ->
      let attrs = token_attrs token in
      let attrs =
        match underlined with
        | true -> Attr.underline :: attrs
        | false -> attrs
      in
      View.text ~attrs text)
  in
  let line =
    Panel.fit
      (View.hcat
         ([ View.text ~attrs:marker_attrs marker; number_view ] @ code))
      ~width
      ~height:1
  in
  match active with
  | true -> View.with_colors' ~fill_backdrop:true ~bg:Theme.accent_bg line
  | false -> line
;;

let first_visible ~lines ~height ~active_line =
  Int.min
    (Int.max 0 (active_line - 1 - (height / 2)))
    (Int.max 0 (lines - height))
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
    let lines = Source_file.length loaded.file in
    let offset = first_visible ~lines ~height ~active_line in
    let visible =
      List.init
        (Int.min height (lines - offset))
        ~f:(fun row ->
          let number = offset + row + 1 in
          code_line
            ~width
            loaded
            ~number
            ~active:(number = active_line)
            ~callsite:
              (match callsite_line with
               | Some line -> line = number
               | None -> false)
            ~char_range)
    in
    View.vcat visible
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
