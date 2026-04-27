open Ast

module Runtime = struct
  module StringMap = Map.Make (String)

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
    | VRecord of value StringMap.t
    | VBuiltin of (value -> value)

  and variant_data = { tag : string; value : value }
  and closure_data = { f : Ast.lambda_body; captured : value StringMap.t }

  type stackframe = { parent : stackframe option; locals : value StringMap.t }

  let rec value_to_string = function
    | VClosure _ -> "<closure>"
    | VString s -> s
    | VInt i -> string_of_int i
    | VFloat f -> string_of_float f
    | VTuple _ -> "<tuple>"
    | VUnit -> "()"
    | VBool b -> if b then "да" else "нет"
    | VVariant v -> Format.sprintf "%s %s" v.tag (value_to_string v.value)
    | VList l ->
        List.map value_to_string l |> String.concat "; "
        |> Format.sprintf "[%s]"
    | VRecord r ->
        let serialize_field (f, v) =
          Format.sprintf "%s = %s" f (value_to_string v)
        in
        let fields =
          StringMap.to_seq r |> List.of_seq |> List.map serialize_field
          |> String.concat "; "
        in
        Format.sprintf "{ %s }" fields
    | VBuiltin _ -> "<builtin>"
end
