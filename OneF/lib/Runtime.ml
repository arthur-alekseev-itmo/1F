open Ast

module Runtime = struct
  module StringMap = Map.Make(String)

  type value =
    | VClosure of closure_data
    | VString of string
    | VInt of int
    | VFloat of float
    | VTuple of value list
    | VUnit
    | VBool of bool
    | VVariant of variant_data
    | VList of value list
    | VRecord of (string, value) Hashtbl.t
    | VBuiltin of (value -> value)

  and variant_data = { tag : int; value : value }
  and closure_data = { f : Ast.lambda_body; captured : value StringMap.t }

  type stackframe = { parent : stackframe option; locals : value StringMap.t }

  let value_to_string = function
    | VClosure _ -> "<closure>"
    | VString s -> s
    | VInt i -> string_of_int i
    | VFloat f -> string_of_float f
    | VTuple _ -> "<tuple>"
    | VUnit -> "()"
    | VBool b -> string_of_bool b
    | VVariant _ -> "<variant>"
    | VList _ -> "<list>"
    | VRecord _ -> "<record>"
    | VBuiltin _ -> "<builtin>"

  let rec print_context ctx =
    let hashtbl_to_string htbl =
      StringMap.fold
        (fun k v acc ->
          if acc = "" then Printf.sprintf "\"%s\": %s" k (value_to_string v)
          else Printf.sprintf "%s, \"%s\": %s" acc k (value_to_string v))
        htbl ""
      |> fun s -> "{ " ^ s ^ " }"
    in
    hashtbl_to_string ctx.locals |> print_endline;
    match ctx.parent with
    | None -> ()
    | Some p -> print_context p
end
