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

(* Consecutive non-space chunks are ONE word even where their tags differ. A
   label and its value arrive as two spans with nothing between them, and a
   line broken at that seam leaves [a=] dangling with its [1] on the next
   line — the text never had a space there, so neither may the wrapping. A
   word wraps whole, or it hard-splits; it does not come apart at a seam that
   is only a change of color. *)
let words chunks =
  List.group chunks ~break:(fun ((_ : _), before) ((_ : _), after) ->
    String.equal before " " || String.equal after " ")
;;

let word_width word =
  List.sum (module Int) word ~f:(fun ((_ : _), text) -> width_of text)
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
    (* as much of an over-long word as one line can hold, and the rest.
       Called only on a fresh line, so what will not fit had nowhere better
       to go — and the cut lands inside a chunk only where there is room for
       at least a column of it, or the line would end up wider than it is
       allowed. *)
    let rec cut word column limit taken =
      match word with
      | [] -> List.rev taken, []
      | ((tag, text) as chunk) :: rest ->
        let chunk_width = width_of text in
        (match column + chunk_width <= limit, column < limit with
         | true, (true | false) ->
           cut rest (column + chunk_width) limit (chunk :: taken)
         | false, false -> List.rev taken, word
         | false, true ->
           let at = split_to_fit text ~limit:(limit - column) in
           ( List.rev ((tag, String.prefix text at) :: taken)
           , (tag, String.drop_prefix text at) :: rest ))
    in
    let rec go words line column limit lines =
      match words with
      | [] -> flush line lines
      | word :: rest ->
        let width_here = word_width word in
        (match column + width_here <= limit with
         | true ->
           go
             rest
             (List.rev_append word line)
             (column + width_here)
             limit
             lines
         | false ->
           (match word with
            (* the space a break lands on IS the break *)
            | [ ((_ : _), " ") ] -> go rest [] 0 width (flush line lines)
            | [] -> go rest line column limit lines
            | (_ : _ list) ->
              (match column = 0 with
               | true ->
                 let head, tail = cut word 0 limit [] in
                 go (tail :: rest) [] 0 width (flush head lines)
               | false -> go (word :: rest) [] 0 width (flush line lines))))
    in
    (match List.rev (go (words (chunks spans)) [] 0 first_width []) with
     | [] -> [ [] ]
     | lines -> lines)
;;
