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

(* Everything perf attributed to one source FILE — its module's named entries
   plus any lambdas defined in it.

   The coarse reading, for a call the profile cannot name. An instrumented
   event fires on a binding inside a function, so its callee often arrives as
   an expression ({!Function_info.Unnamed}) whose location is a line in the
   middle of that function — no name to match, and no lambda defined there
   either, so {!share} rightly declines. But the file is known, perf knows
   what that file's code cost, and "this call is in the module the run spends
   its time in" is a true and useful thing to say. Per-file, never per-call:
   two calls in one file share a value. *)
let file_samples t ~location =
  let call_file = Filename.basename (Location.file_path location) in
  let module_name = String.capitalize (Filename.chop_extension call_file) in
  let samples =
    List.sum (module Int) t.entries ~f:(fun entry ->
      match entry.kind with
      | Kind.Anonymous { file_path; _ } ->
        (match String.equal (Filename.basename file_path) call_file with
         | true -> entry.samples
         | false -> 0)
      | Kind.Named (_ : string) ->
        (match List.last entry.module_path with
         | Some last when String.equal last module_name -> entry.samples
         | Some (_ : string) | None -> 0))
  in
  match samples with 0 -> None | samples -> Some samples
;;

let file_share t ~location =
  let total = total_samples t in
  match total with
  | 0 -> None
  | _ ->
    Option.map (file_samples t ~location) ~f:(fun samples ->
      Float.of_int samples /. Float.of_int total)
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
