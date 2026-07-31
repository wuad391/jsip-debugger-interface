open! Core

type t =
  { id : int
  ; address : Snapshot.Address.t
  ; name : string option
  }

(* the wire writes a named entry as [(1 0x7f2ce89e q)] and an anonymous one
   as [(1 0x7f2ce89e)] — see the compiler's [Vreplay.Sexp. sexp_of_registry] *)
let t_of_sexp sexp =
  match (sexp : Sexp.t) with
  | List [ id; address ] ->
    { id = [%of_sexp: int] id
    ; address = [%of_sexp: Snapshot.Address.t] address
    ; name = None
    }
  | List [ id; address; name ] ->
    { id = [%of_sexp: int] id
    ; address = [%of_sexp: Snapshot.Address.t] address
    ; name = Some ([%of_sexp: string] name)
    }
  | List _ | Atom _ ->
    raise_s
      [%message
        "Registry_entry: expected (id address) or (id address name)"
          (sexp : Sexp.t)]
;;

let sexp_of_t { id; address; name } =
  let base =
    [ [%sexp_of: int] id; [%sexp_of: Snapshot.Address.t] address ]
  in
  match name with
  | None -> Sexp.List base
  | Some name -> Sexp.List (base @ [ [%sexp_of: string] name ])
;;

(* what the interface prints for the structure: its latest variable name, or
   its registry id when it was never observed under one *)
let display t =
  match t.name with Some name -> name | None -> [%string "#%{t.id#Int}"]
;;
