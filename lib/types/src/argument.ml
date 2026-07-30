open! Core

module T = struct
  type t =
    | No_label of { expression : Function_info.t }
    | Labelled of
        { label : string
        ; expression : Function_info.t
        }
    | Optional of
        { label : string
        ; expression : Function_info.t
        }
  [@@deriving sexp, bin_io]
end

include T

(* The compiler prints each argument as bare source text: an argument the
   application was abstracted over reads OMITTED (shown the way the missing
   argument reads at the call site), and a compound expression like [v * 2]
   carries no parentheses of its own, so it gets them back to read
   unambiguously in a caption. *)
let display_expression expression =
  match Function_info.display expression with
  | "OMITTED" -> "_"
  | source ->
    let is_delimited =
      match String.to_list source with
      | ('(' | '[' | '{' | '"') :: _ -> true
      | _ -> false
    in
    (match String.contains source ' ' && not is_delimited with
     | true -> [%string "(%{source})"]
     | false -> source)
;;

let display t =
  match t with
  | No_label { expression } -> display_expression expression
  | Labelled { label; expression } ->
    [%string "~%{label}:%{display_expression expression}"]
  | Optional { label; expression } ->
    [%string "?%{label}:%{display_expression expression}"]
;;
