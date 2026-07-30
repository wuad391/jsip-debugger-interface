open! Core
open Jsip_types

module Token = struct
  type t =
    | Keyword
    | Uident
    | String
    | Number
    | Comment
    | Operator
    | Plain
  [@@deriving sexp_of, compare, equal]
end

let keywords =
  String.Set.of_list
    [ "and"
    ; "as"
    ; "assert"
    ; "begin"
    ; "do"
    ; "done"
    ; "downto"
    ; "else"
    ; "end"
    ; "exception"
    ; "external"
    ; "false"
    ; "for"
    ; "fun"
    ; "function"
    ; "functor"
    ; "if"
    ; "in"
    ; "include"
    ; "inherit"
    ; "lazy"
    ; "let"
    ; "match"
    ; "method"
    ; "module"
    ; "mutable"
    ; "new"
    ; "nonrec"
    ; "object"
    ; "of"
    ; "open"
    ; "private"
    ; "rec"
    ; "sig"
    ; "struct"
    ; "then"
    ; "to"
    ; "true"
    ; "try"
    ; "type"
    ; "val"
    ; "virtual"
    ; "when"
    ; "while"
    ; "with"
    ]
;;

let is_word_start c = Char.is_alpha c || Char.equal c '_'
let is_word c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '\''
let is_operator c = String.mem "+-*/=<>|&^!?~@:;,.()[]{}#" c

(* one line, threading the surrounding [(* *)] nesting depth through *)
let line ~comment_depth text =
  let spans = Queue.create () in
  let flush kind start stop =
    match stop > start with
    | true ->
      Queue.enqueue
        spans
        (kind, String.sub text ~pos:start ~len:(stop - start))
    | false -> ()
  in
  let length = String.length text in
  let depth = ref comment_depth in
  let position = ref 0 in
  while !position < length do
    let start = !position in
    match !depth > 0 with
    | true ->
      (* consume comment text until this line ends or the depth changes *)
      let rec scan i =
        match i + 1 < length with
        | false -> length
        | true ->
          (match text.[i], text.[i + 1] with
           | '(', '*' ->
             depth := !depth + 1;
             scan (i + 2)
           | '*', ')' ->
             depth := !depth - 1;
             (match !depth with 0 -> i + 2 | _ -> scan (i + 2))
           | _ -> scan (i + 1))
      in
      let stop = scan start in
      position := stop;
      flush Token.Comment start stop
    | false ->
      let c = text.[start] in
      (match c with
       | '(' when start + 1 < length && Char.equal text.[start + 1] '*' ->
         (* emit the opener; the comment branch handles the rest *)
         depth := 1;
         position := start + 2;
         flush Token.Comment start (start + 2)
       | '"' ->
         let rec scan i =
           match i < length with
           | false -> length
           | true ->
             (match text.[i] with
              | '\\' -> scan (i + 2)
              | '"' -> i + 1
              | _ -> scan (i + 1))
         in
         let stop = scan (start + 1) in
         position := stop;
         flush Token.String start stop
       | c when Char.is_digit c ->
         let rec scan i =
           match
             i < length
             && (Char.is_alphanum text.[i]
                 || Char.equal text.[i] '.'
                 || Char.equal text.[i] '_')
           with
           | true -> scan (i + 1)
           | false -> i
         in
         let stop = scan start in
         position := stop;
         flush Token.Number start stop
       | c when is_word_start c ->
         let rec scan i =
           match i < length && is_word text.[i] with
           | true -> scan (i + 1)
           | false -> i
         in
         let stop = scan start in
         let word = String.sub text ~pos:start ~len:(stop - start) in
         let kind =
           match Set.mem keywords word with
           | true -> Token.Keyword
           | false ->
             (match Char.is_uppercase c with
              | true -> Token.Uident
              | false -> Token.Plain)
         in
         position := stop;
         flush kind start stop
       | c when is_operator c ->
         let rec scan i =
           match
             i < length
             && is_operator text.[i]
             && not
                  (Char.equal text.[i] '('
                   && i + 1 < length
                   && Char.equal text.[i + 1] '*')
           with
           | true -> scan (i + 1)
           | false -> i
         in
         let stop = max (scan start) (start + 1) in
         position := stop;
         flush Token.Operator start stop
       | _ ->
         let rec scan i =
           match i < length && Char.is_whitespace text.[i] with
           | true -> scan (i + 1)
           | false -> i
         in
         let stop = max (scan start) (start + 1) in
         position := stop;
         flush Token.Plain start stop)
  done;
  Queue.to_list spans, !depth
;;

let file source =
  let depth = ref 0 in
  Array.init (Source_file.length source) ~f:(fun index ->
    let text =
      Option.value_exn (Source_file.line source ~number:(index + 1))
    in
    let spans, next_depth = line ~comment_depth:!depth text in
    depth := next_depth;
    spans)
;;
