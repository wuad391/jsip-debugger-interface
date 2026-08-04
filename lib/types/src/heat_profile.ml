open! Core

module Kind = struct
  type t =
    | Named of string
    | Anonymous of
        { file_path : string
        ; line_number : int
        ; char_range : int * int
        }
  [@@deriving sexp, equal]
end

module Entry = struct
  type t =
    { module_path : string list
    ; kind : Kind.t
    ; samples : int
    }
  [@@deriving sexp]
end

type t =
  { version : int
  ; root_module : string
  ; entries : Entry.t list
  }
[@@deriving sexp] [@@sexp.allow_extra_fields]

let total_samples t =
  List.sum (module Int) t.entries ~f:(fun entry -> entry.samples)
;;

(* candidates for a lambda: anonymous entries defined on the call's line. The
   trace only knows the call site and perf only knows the definition site;
   for a directly applied lambda they share a line, and an exact or
   overlapping character range disambiguates several on one line. *)
let anonymous_samples t ~location =
  let call_file = Filename.basename (Location.file_path location) in
  let call_line = Location.line_number location in
  let call_range = Location.char_range location in
  let on_call_line =
    List.filter_map t.entries ~f:(fun entry ->
      match entry.kind with
      | Kind.Named _ -> None
      | Kind.Anonymous { file_path; line_number; char_range } ->
        (match
           String.equal (Filename.basename file_path) call_file
           && line_number = call_line
         with
         | true -> Some (entry.samples, char_range)
         | false -> None))
  in
  let sum candidates =
    List.sum (module Int) candidates ~f:(fun (samples, _) -> samples)
  in
  match on_call_line with
  | [] -> None
  | _ ->
    let exact =
      List.filter on_call_line ~f:(fun (_, range) ->
        [%equal: int * int] range call_range)
    in
    let overlapping =
      let call_start, call_end = call_range in
      List.filter on_call_line ~f:(fun (_, (start_, end_)) ->
        start_ <= call_end && call_start <= end_)
    in
    (match exact, overlapping with
     | _ :: _, _ -> Some (sum exact)
     | [], _ :: _ -> Some (sum overlapping)
     | [], [] -> Some (sum on_call_line))
;;

let named_samples t ~name =
  let qualifier, base =
    match String.rsplit2 name ~on:'.' with
    | Some (qualifier, base) -> Some qualifier, base
    | None -> None, name
  in
  let candidates =
    List.filter t.entries ~f:(fun entry ->
      match entry.kind with
      | Kind.Named entry_name -> String.equal entry_name base
      | Kind.Anonymous _ -> false)
  in
  match candidates with
  | [] -> None
  | _ :: _ ->
    let restricted =
      match qualifier with
      | Some qualifier ->
        (* "Map.fold" restricts to ["Stdlib"; "Map"]-style paths; a qualifier
           that resolves nowhere is an alias ("M" for [Map.Make (String)])
           and restricts nothing *)
        let suffix = String.split qualifier ~on:'.' in
        (match
           List.filter candidates ~f:(fun entry ->
             List.is_suffix entry.module_path ~suffix ~equal:String.equal)
         with
         | [] -> candidates
         | qualified -> qualified)
      | None ->
        (* an unqualified name is most likely the user's own function; only
           fall back to same-named stdlib entries when the user's module has
           none *)
        (match
           List.filter candidates ~f:(fun entry ->
             match entry.module_path with
             | root :: _ -> String.equal root t.root_module
             | [] -> false)
         with
         | [] -> candidates
         | own -> own)
    in
    Some (List.sum (module Int) restricted ~f:(fun entry -> entry.samples))
;;

let share t ~function_info ~location =
  let total = total_samples t in
  match total with
  | 0 -> None
  | _ ->
    let samples =
      match (function_info : Function_info.t) with
      | Unnamed _ -> anonymous_samples t ~location
      | Function_name name -> named_samples t ~name
    in
    Option.map samples ~f:(fun samples ->
      Float.of_int samples /. Float.of_int total)
;;
