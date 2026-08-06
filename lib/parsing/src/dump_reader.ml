open! Core

let depth_change depth_update =
  let change = ref 0 in
  String.iter depth_update ~f:(fun char ->
    match char with
    | '{' -> change := !change + 1
    | '}' -> change := !change - 1
    | _ -> ());
  !change
;;

(* looks like:
   "[{(event (id 1) (loc ...) (fn ...) (args ...) (registry ...) (snapshot ...))" -- the {}]
   marker prefix carries the depth delta, the rest of the line is one event
   sexp that [Dump_wire] reads directly. A line with no payload is a bare
   marker, which is only valid as the dump returning to depth 0. *)
let parse_line line ~current_depth =
  match String.index line '(' with
  | None ->
    current_depth := !current_depth + depth_change line;
    (match !current_depth with
     | 0 -> Ok None
     | depth ->
       Or_error.error_s
         [%message
           "dump does not return to depth 0" (depth : int) (line : string)])
  | Some payload_start ->
    let markers = String.prefix line payload_start in
    let payload = String.drop_prefix line payload_start in
    current_depth := !current_depth + depth_change markers;
    let depth = !current_depth in
    Or_error.map (Dump_wire.of_string payload) ~f:(fun wire ->
      Some (Dump_wire.to_call_info wire ~depth))
;;

(* one fold over lines, shared by the file and in-memory entry points, so a
   dump fetched over HTTP parses exactly as one read off disk *)
let parse_lines lines =
  let current_depth = ref 0 in
  let parsed = Queue.create () in
  List.fold lines ~init:(Ok 1) ~f:(fun acc line ->
    match acc with
    | Error _ as error -> error
    | Ok line_number ->
      (match parse_line line ~current_depth with
       (* the position is the whole diagnostic for a malformed dump, so it is
          attached here rather than left to the caller *)
       | Error error ->
         Error (Error.tag_s error ~tag:[%message (line_number : int)])
       | Ok None -> Ok (line_number + 1)
       | Ok (Some info) ->
         Queue.enqueue parsed info;
         Ok (line_number + 1)))
  |> Or_error.map ~f:(fun (_ : int) -> parsed)
;;

let parse contents = parse_lines (String.split_lines contents)

let read file_path =
  Or_error.join
    (Or_error.try_with (fun () ->
       In_channel.with_file file_path ~f:(fun channel ->
         parse_lines (In_channel.input_lines channel))))
;;
