open! Core
open Jsip_types

(* The running registry the deltas fold into: entry contents by id, plus ids
   in first-upsert order, because the wire's full echoes listed entries in
   registry (insertion) order and the fold must reproduce them exactly — ids
   alone cannot (a structure first dumped as an interior member keeps its
   older wire id when it later becomes a tracked root, so insertion order is
   not id order). An upsert of a known id updates in place; a dropped id goes
   inert in [order] and is skipped when materializing. *)
module Folded_registry = struct
  type t =
    { entries : Registry_entry.t Hashtbl.M(Int).t
    ; mutable order : int list (* reversed: latest first *)
    }

  let create () = { entries = Hashtbl.create (module Int); order = [] }

  let apply t ({ upserts; drops } : Registry_delta.t) =
    List.iter upserts ~f:(fun (entry : Registry_entry.t) ->
      if not (Hashtbl.mem t.entries entry.id)
      then t.order <- entry.id :: t.order;
      Hashtbl.set t.entries ~key:entry.id ~data:entry);
    List.iter drops ~f:(fun id -> Hashtbl.remove t.entries id)
  ;;

  (* a full echo is authoritative: a transitional dump carrying both forms
     keeps folding correctly after it *)
  let reset t registry =
    Hashtbl.clear t.entries;
    t.order <- [];
    List.iter registry ~f:(fun (entry : Registry_entry.t) ->
      t.order <- entry.id :: t.order;
      Hashtbl.set t.entries ~key:entry.id ~data:entry)
  ;;

  (* [order] is latest-first, so the reversing filter_map lands on
     first-upsert order — the order the full echoes listed *)
  let to_list t = List.rev_filter_map t.order ~f:(Hashtbl.find t.entries)
end

let depth_change depth_update =
  let change = ref 0 in
  String.iter depth_update ~f:(fun char ->
    match char with
    | '{' -> change := !change + 1
    | '}' -> change := !change - 1
    | _ -> ());
  !change
;;

(* The event's whole registry, off whichever form the wire carried. A full
   [registry] wins and resets the fold (dumps up to compiler PR #23 state one
   per event, and a transitional compiler emitting both forms stays
   coherent); a [registry_delta] folds into the previous event's; an event
   carrying neither is from neither wire and is malformed. *)
let resolve_registry (wire : Dump_wire.t) ~folded =
  match wire.registry, wire.registry_delta with
  | Some registry, (Some _ | None) ->
    Folded_registry.reset folded registry;
    Ok registry
  | None, Some delta ->
    Folded_registry.apply folded delta;
    Ok (Folded_registry.to_list folded)
  | None, None ->
    Or_error.error_s
      [%message
        "event carries neither registry nor registry_delta"
          ~event_id:(wire.id : int)]
;;

(* looks like:
   "[{(event (id 1) (loc ...) (fn ...) (args ...) (registry_delta ...) (snapshot ...))" -- the {}]
   marker prefix carries the depth delta, the rest of the line is one event
   sexp that [Dump_wire] reads directly. A line with no payload is a bare
   marker, which is only valid as the dump returning to depth 0. *)
let parse_line line ~current_depth ~folded =
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
    let%bind.Or_error wire = Dump_wire.of_string payload in
    let%map.Or_error registry = resolve_registry wire ~folded in
    Some (Dump_wire.to_call_info wire ~depth ~registry)
;;

(* one fold over lines, shared by the file and in-memory entry points, so a
   dump fetched over HTTP parses exactly as one read off disk *)
let parse_lines lines =
  let current_depth = ref 0 in
  let folded = Folded_registry.create () in
  let parsed = Queue.create () in
  List.fold lines ~init:(Ok 1) ~f:(fun acc line ->
    match acc with
    | Error _ as error -> error
    | Ok line_number ->
      (match parse_line line ~current_depth ~folded with
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
