open! Core
module View = Bonsai_term.View

(* Terminal columns, not bytes: the panes wrap walked strings, box-drawing
   guides and arrows, where the two disagree — [→] is three bytes in one
   cell, and counting bytes would break a line a third of the way early. This
   is the renderer's own measure, so wrapping and drawing cannot fall out of
   step. *)
let width_of text = View.width (View.text text)

(* split one span into word and whitespace chunks, tags preserved *)
let chunks spans =
  List.concat_map spans ~f:(fun (tag, text) ->
    String.split text ~on:' '
    |> List.map ~f:(fun word -> tag, word)
    |> List.intersperse ~sep:(tag, " "))
  |> List.filter ~f:(fun ((_ : _), text) -> not (String.is_empty text))
;;

(* How much of [text] fits in [limit] columns, cut on a code-point boundary
   so a hard split never lands inside a glyph. Always at least one code
   point: a character wider than the whole line still has to go somewhere, or
   the split would never advance. *)
let split_to_fit text ~limit =
  let length = String.length text in
  let is_boundary index =
    index >= length || Char.to_int text.[index] land 0xc0 <> 0x80
  in
  let rec boundary_after index =
    match is_boundary (index + 1) with
    | true -> index + 1
    | false -> boundary_after (index + 1)
  in
  let rec go cut column =
    match cut >= length with
    | true -> cut
    | false ->
      let after = boundary_after cut in
      let column =
        column + width_of (String.sub text ~pos:cut ~len:(after - cut))
      in
      (match column <= limit with true -> go after column | false -> cut)
  in
  match go 0 0 with 0 -> boundary_after 0 | cut -> cut
;;

let spans ?first_width spans ~width =
  let first_width = Option.value first_width ~default:width in
  match width < 2 || first_width < 2 with
  | true -> [ spans ]
  | false ->
    let flush line lines =
      match List.is_empty line with
      | true -> lines
      | false -> List.rev line :: lines
    in
    let rec go chunks line column limit lines =
      match chunks with
      | [] -> flush line lines
      | ((tag, text) as chunk) :: rest ->
        let length = width_of text in
        (match column + length <= limit with
         | true ->
           go rest ((tag, text) :: line) (column + length) limit lines
         | false ->
           (* break here: drop the whitespace the break lands on, and
              hard-split a word that cannot fit even on a fresh line *)
           (match String.equal text " " with
            | true -> go rest [] 0 width (flush line lines)
            | false ->
              (match column = 0 with
               | true ->
                 let cut = split_to_fit text ~limit in
                 let head = tag, String.prefix text cut in
                 let tail = tag, String.drop_prefix text cut in
                 go (tail :: rest) [] 0 width (flush [ head ] lines)
               | false -> go (chunk :: rest) [] 0 width (flush line lines))))
    in
    (match List.rev (go (chunks spans) [] 0 first_width []) with
     | [] -> [ [] ]
     | lines -> lines)
;;
