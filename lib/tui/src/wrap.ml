open! Core

(* split one span into word and whitespace chunks, tags preserved *)
let chunks spans =
  List.concat_map spans ~f:(fun (tag, text) ->
    String.split text ~on:' '
    |> List.map ~f:(fun word -> tag, word)
    |> List.intersperse ~sep:(tag, " "))
  |> List.filter ~f:(fun ((_ : _), text) -> not (String.is_empty text))
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
        let length = String.length text in
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
                 let head = tag, String.prefix text limit in
                 let tail = tag, String.drop_prefix text limit in
                 go (tail :: rest) [] 0 width (flush [ head ] lines)
               | false -> go (chunk :: rest) [] 0 width (flush line lines))))
    in
    (match List.rev (go (chunks spans) [] 0 first_width []) with
     | [] -> [ [] ]
     | lines -> lines)
;;
