open! Core
open Jsip_types
module Syntax = Jsip_parsing.Syntax

module Loaded = struct
  type t =
    { file : Source_file.t
    ; spans : (Syntax.Token.t * string) list Array.t
    ; regions : (int * int) list
    ; regions_by_start : int Int.Map.t
    }

  (* a foldable region: a top-level definition — a line starting at column 0
     — and everything under it until the next one. Only regions with
     something to hide (two lines or more) fold. The TUI's reading, exactly
     ({!Jsip_tui_components.Source_pane.Loaded}). *)
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

(* split one line's spans so the event's [char_range] can carry its own
   marking — [(token, text, marked)] pieces whose texts concatenate back to
   the line *)
let mark_range spans ~char_range:(range_start, range_stop) =
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

module Row = struct
  type t =
    | Code of
        { number : int
        ; is_active : bool
        ; is_callsite : bool
        ; region_start : bool (** a fold glyph belongs in the gutter *)
        ; folded : bool
        ; spans : (Syntax.Token.t * string * bool) list
        (** [(token, text, in_char_range)] *)
        }
    | Folded_marker of
        { start : int
        ; stop : int
        ; hides_active : bool
        (** the fold is standing in for the active line, so it takes the
            wash in its place *)
        }
  [@@deriving sexp_of]
end

(* the pane's rows: every line of the file, folded regions collapsed to
   their first line plus a [⋯ n lines] marker *)
let rows (loaded : Loaded.t) ~folds ~active_line ~callsite_line ~char_range =
  let length = Source_file.length loaded.file in
  let rec walk number acc =
    match number > length with
    | true -> List.rev acc
    | false ->
      let region = Map.find loaded.regions_by_start number in
      let folded =
        match region with
        | Some (_ : int) -> Set.mem folds number
        | None -> false
      in
      let is_active = number = active_line in
      let spans =
        let spans =
          match number - 1 < Array.length loaded.spans with
          | true -> loaded.spans.(number - 1)
          | false -> []
        in
        match is_active with
        | true -> mark_range spans ~char_range
        | false ->
          List.map spans ~f:(fun (token, text) -> token, text, false)
      in
      let row =
        Row.Code
          { number
          ; is_active
          ; is_callsite =
              (match callsite_line with
               | Some callsite -> number = callsite && not is_active
               | None -> false)
          ; region_start = Option.is_some region
          ; folded
          ; spans
          }
      in
      (match folded, region with
       | true, Some stop ->
         let hides_active = active_line > number && active_line <= stop in
         walk
           (stop + 1)
           (Row.Folded_marker { start = number; stop; hides_active }
            :: row
            :: acc)
       | (true | false), (Some (_ : int) | None) ->
         walk (number + 1) (row :: acc))
  in
  walk 1 []
;;
