open Ast
open Runtime.Runtime
open Builtins

module Interpreter = struct
  let ( >>= ) = Result.bind

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
    | PatListCons (ph, pt), VList (vh :: vt) ->
        set_pattern ph vh vars >>= fun vars' -> set_pattern pt (VList vt) vars'
    | PatWildcard, _ -> Ok vars
    | PatUnit, VUnit -> Ok vars
    | PatVariable name, value -> Ok (StringMap.add name value vars)
    | PatTuple ps, VTuple vs ->
        let zipped = List.combine ps vs in
        let folder vars (k, v) = vars >>= fun vars -> set_pattern k v vars in
        List.fold_left folder (Ok vars) zipped
    | PatCtor (pname, ppat), VVariant v when pname = v.tag ->
        set_pattern ppat v.value vars
    | _ -> Error "Bad pattern match"

  let set_pattern_force p e v =
    match set_pattern p e v with Ok ok -> ok | Error err -> failwith err

  let set_pattern_to_ctx (p : Ast.pattern) (e : value) ctx =
    let locals = set_pattern_force p e ctx.locals in
    { ctx with locals }

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
    | Lambda lam ->
        VClosure { f = lam; captured = ctx.locals } (* Сохранять ссылочку  *)
    | IfThenElse ite -> eval_ite ite ctx
    | TupleInit xs ->
        let xs' = List.map (fun e -> eval_expr e ctx) xs in
        VTuple xs'
    | Ctor c -> VVariant { tag = c; value = VUnit }
    | FieldAccess (target, field) ->
        let target' = eval_expr target ctx in
        eval_field_access target' field ctx
    | Match (scrutinee, branches) ->
        let scrutinee' = eval_expr scrutinee ctx in
        eval_match scrutinee' branches ctx
    | RecordInit fields ->
        let kvs = List.map (fun (k, v) -> (k, eval_expr v ctx)) fields in
        kvs |> List.to_seq |> StringMap.of_seq |> fun m -> VRecord m
    | RecordUpdate _ -> failwith "TODO"
    | EmptyList -> VList []

  and eval_match (scrutinee : value) branches ctx =
    let try_branch (branch : Ast.match_pattern_branch) =
      match set_pattern branch.pattern scrutinee ctx.locals with
      | Ok locals ->
          let ctx' = { ctx with locals } in
          let t = Ast.Const (BoolLiteral true) in
          let when_clause = Option.value ~default:t branch.when_clause in
          let guard_result = eval_expr when_clause ctx' in
          if guard_result = VBool true then Ok (eval_expr branch.result ctx')
          else Error "Guard failed"
      | Error e -> Error e
    in
    match branches with
    | [] -> failwith "Non exhaustive match"
    | h :: t -> (
        match try_branch h with
        | Ok v -> v
        | Error _ -> eval_match scrutinee t ctx)

  and eval_ite (ite : Ast.ite_body) ctx =
    let cond' = eval_expr ite.cond ctx in
    match cond' with
    | VBool true -> eval_expr ite.thenBranch ctx
    | VBool false -> eval_expr ite.elseBranch ctx
    | _ -> failwith "Awaited bool"

  and eval_field_access (target : value) (field : string) _ =
    match target with
    | VRecord r -> StringMap.find field r
    | _ -> failwith "Can only lookup value in the record"

  and eval_application (callee : value) (arg : value) ctx =
    match callee with
    | VBuiltin b -> b arg
    | VClosure closure ->
        let cap' = set_pattern_force closure.f.arg arg closure.captured in
        let ctx' = { parent = Some ctx; locals = cap' } in
        eval_expr closure.f.body ctx'
    | VVariant v when v.value = VUnit -> VVariant { v with value = arg }
    | _ -> failwith "Cannot apply non-function"

  let interpret_decl ctx (d : Ast.decl) =
    let value' = eval_expr d.body ctx in
    set_pattern_to_ctx d.name value' ctx

  let interpret (p : Ast.program) =
    List.fold_left interpret_decl initial_stack p

  let eval_string s =
    let run = function
      | Parser.Parser.Parsed (r, []) ->
          eval_expr r initial_stack |> value_to_string |> print_endline
      | Parser.Parser.Failed f -> failwith f
      | Parser.Parser.Parsed (_, t) ->
          let message =
            List.map Lexemes.Lexemes.to_string t |> String.concat "; "
          in
          failwith message
      | _ -> failwith "TODO"
    in
    Lexer.Lexer.lex_string s
    |> Result.map (fun r -> List.map (fun (x, _, _) -> x) r)
    |> Result.map (fun x -> Parser.Parser.parse_expr x)
    |> Result.iter run
end
