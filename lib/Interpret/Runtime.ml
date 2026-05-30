open Parsing.Ast
open Parsing.PPAst

module Runtime = struct
  module StringMap = Map.Make (String)

  type value =
    | VModule of module_data
    | VClosure of closure_data
    | VString of CCUtf8_string.t
    | VInt of int
    | VFloat of float
    | VTuple of value list
    | VUnit
    | VBool of bool
    | VVariant of variant_data
    | VList of value list
    | VRecord of value StringMap.t
    | VBuiltin of (value -> value)
    | VChar of Uchar.t
    | VLazy of Ast.expr * value StringMap.t * Ast.pattern option

  and variant_data = { tag : string; value : value }
  and closure_data = { f : Ast.lambda_body; captured : value StringMap.t }
  and module_data = { name : string; values : value StringMap.t }
  and stackframe = { parent : stackframe option; locals : value StringMap.t }

  let uchar_to_string u =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Buffer.contents b


  let not_builtin (_, v) = match v with VBuiltin _ -> false | _ -> true

  let rec value_to_string = function
    | VClosure closure ->
        let mapping (k, v) = Fmt.str "(%s = %s)" k (value_to_string v) in
        closure.captured
        |> StringMap.to_list
        |> List.filter not_builtin
        |> List.map mapping
        |> String.concat ", "
        |> Fmt.str "<closure, fn=%s, cap=%s>"
             (PPAst.pp_expr (Lambda closure.f, Unknown))
    | VString s -> CCUtf8_string.to_string s
    | VInt i -> string_of_int i
    | VFloat f -> string_of_float f
    | VTuple t ->
        List.map value_to_string t
        |> String.concat ", "
        |> Format.sprintf "(%s)"
    | VUnit -> "()"
    | VBool b -> if b then "да" else "нет"
    | VVariant v -> Format.sprintf "%s %s" v.tag (value_to_string v.value)
    | VList l ->
        List.map value_to_string l
        |> String.concat "; "
        |> Format.sprintf "[%s]"
    | VRecord r ->
        let serialize_field (f, v) =
          Format.sprintf "%s = %s" f (value_to_string v)
        in
        let fields =
          StringMap.to_seq r
          |> List.of_seq
          |> List.map serialize_field
          |> String.concat "; "
        in
        Format.sprintf "{ %s }" fields
    | VBuiltin _ -> "<builtin>"
    | VChar c -> uchar_to_string c
    | VLazy (e, _, _) -> Fmt.str "<lazy %s>" (PPAst.pp_expr e)
    | VModule m -> Format.sprintf "<module %s>" m.name


  let rec stackframe_to_string (frame : stackframe) =
    let not_builtin (_, v) = match v with VBuiltin _ -> false | _ -> true in
    let mapping (k, v) = Fmt.str "(%s = %s)" k (value_to_string v) in
    let locals =
      frame.locals
      |> StringMap.to_list
      |> List.filter not_builtin
      |> List.map mapping
      |> String.concat "\n"
    in
    let parent =
      Option.map stackframe_to_string frame.parent |> Option.value ~default:""
    in
    Fmt.str "%s\n -----------\n %s" locals parent
end
