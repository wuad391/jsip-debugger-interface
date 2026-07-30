open! Core

(* split one span into word and whitespace chunks, tags preserved *)
let chunks spans =
  List.concat_map spans ~f:(fun (tag, text) ->
    String.split text ~on:' '
    |> List.map ~f:(fun word -> tag, word)
    |> List.intersperse ~sep:(tag, " "))
  |> List.filter ~f:(fun ((_ : _), text) -> not (String.is_empty text))
;;

let hard_split (tag, text) ~width =
  let rec split text pieces =
    match String.length text <= width with
    | true -> List.rev ((tag, text) :: pieces)
    | false ->
      split
        (String.drop_prefix text width)
        ((tag, String.prefix text width) :: pieces)
  in
  split text []
;;

let spans spans ~width =
  match width < 2 with
  | true -> [ spans ]
  | false ->
    let flush line lines =
      match List.is_empty line with
      | true -> lines
      | false -> List.rev line :: lines
    in
    let lines, line, (_ : int) =
      List.fold
        (chunks spans)
        ~init:([], [], 0)
        ~f:(fun (lines, line, column) (tag, text) ->
          let length = String.length text in
          match column + length <= width with
          | true -> lines, (tag, text) :: line, column + length
          | false ->
            (* drop the whitespace a break lands on; hard-split a word that
               cannot fit even on its own line *)
            (match String.equal text " " with
             | true -> flush line lines, [], 0
             | false ->
               (match length > width with
                | true ->
                  let pieces = hard_split (tag, text) ~width in
                  let full, last =
                    List.drop_last_exn pieces, List.last_exn pieces
                  in
                  let lines =
                    List.fold
                      full
                      ~init:(flush line lines)
                      ~f:(fun lines piece -> [ piece ] :: lines)
                  in
                  lines, [ last ], String.length (snd last)
                | false -> flush line lines, [ tag, text ], length)))
    in
    (match List.rev (flush line lines) with [] -> [ [] ] | lines -> lines)
;;
