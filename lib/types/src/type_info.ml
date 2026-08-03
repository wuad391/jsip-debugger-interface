open! Core

type t =
  { printed : string
  ; params : (string * string) list
  }
[@@deriving sexp, compare, equal]

let display t =
  let find role = List.Assoc.find t.params role ~equal:String.equal in
  match find "key", find "data" with
  | Some key, Some data -> [%string "⟨%{key} ⇒ %{data}⟩"]
  | Some _, None | None, Some _ | None, None ->
    (match find "elt" with
     | Some elt -> [%string "⟨%{elt}⟩"]
     | None -> [%string "⟨%{t.printed}⟩"])
;;
