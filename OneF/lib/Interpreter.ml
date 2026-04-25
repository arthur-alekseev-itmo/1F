open Ast
open Runtime.Runtime
open Builtins

module Interpreter = struct
  let write_local name value ctx =
    { ctx with locals = StringMap.add name value ctx.locals }

  let rec search_in_ctx (name : string) ctx =
    match (StringMap.find_opt name ctx.locals, ctx.parent) with
    | None, None -> failwith @@ "Value not found : " ^ name
    | Some x, _ -> x
    | None, Some p -> search_in_ctx name p

  let initial_stack = { parent = None; locals = Builtins.builtins }

  let eval_literal (e : Ast.literal) =
    match e with
    | IntLiteral x -> VInt x
    | StringLiteral x -> VString x
    | BoolLiteral x -> VBool x
    | UnitLiteral -> VUnit
    | FloatLiteral x -> VFloat x

  let rec set_pattern (p : Ast.pattern) (e : value) vars =
    match (p, e) with
    | PatUnit, VUnit -> vars
    | PatVariable name, value -> StringMap.add name value vars
    | PatTuple ps, VTuple vs ->
        List.combine ps vs
        |> List.fold_left (fun vars (a, b) -> set_pattern a b vars) vars
    | _ -> failwith "Bad pattern match"

  let set_pattern_to_ctx (p : Ast.pattern) (e : value) ctx =
    let locals = set_pattern p e ctx.locals in {ctx with locals}

  let rec eval_expr (e : Ast.expr) ctx =
    match e with
    | Const lit -> eval_literal lit
    | Application (callee, arg) ->
        let callee' = eval_expr callee ctx in
        let arg' = eval_expr arg ctx in
        eval_application callee' arg' ctx
    | Value name -> search_in_ctx name ctx
    | LetIn (_recursive, pat, expr, body) ->
        (* TODO: REC *)
        let expr' = eval_expr expr ctx in
        let ctx' = set_pattern_to_ctx pat expr' ctx in
        eval_expr body ctx'
    | Lambda lam -> VClosure { f = lam; captured = ctx.locals } (* Сохранять ссылочку  *)
    | IfThenElse ite -> eval_ite ite ctx
    | TupleInit xs ->
        let xs' = List.map (fun e -> eval_expr e ctx) xs in
        VTuple xs'

  and eval_ite (ite : Ast.ite_body) ctx =
    let cond' = eval_expr ite.cond ctx in
    match cond' with
    | VBool true -> eval_expr ite.thenBranch ctx
    | VBool false -> eval_expr ite.elseBranch ctx
    | _ -> failwith "Awaited bool"

  and eval_application (callee : value) (arg : value) ctx =
    match callee with
    | VBuiltin b -> b arg
    | VClosure closure ->
        let _ = set_pattern closure.f.arg arg closure.captured in
        let ctx' = { parent = Some ctx; locals = closure.captured } in
        eval_expr closure.f.body ctx'
    | _ -> failwith "Cannot apply non-function"

  let interpret_decl ctx (d : Ast.decl) =
    let value' = eval_expr d.body ctx in
    set_pattern_to_ctx d.name value' ctx
    

  let interpret (p : Ast.program) = List.fold_left interpret_decl initial_stack p

  let eval_string s =
    let run = function
      | Parser.Parser.Parsed (r, _) ->
          eval_expr r initial_stack |> value_to_string |> print_endline
      | _ -> failwith "Unparsed"
    in
    Lexer.Lexer.lex_string s
    |> Result.map (fun r -> List.map (fun (x, _, _) -> x) r)
    |> Result.map (fun x -> Parser.Parser.parse_expr x)
    |> Result.iter run
end
