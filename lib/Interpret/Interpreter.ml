open Runtime.Runtime
open Parsing.Ast
open Parsing.PPAst
open Runtime.Runtime
open Builtins
open Backend.SkbClosure.SkbClosure

module Interpreter = struct
  let ( >>= ) = Result.bind

  let write_local name value ctx =
    { ctx with locals = StringMap.add name value ctx.locals }


  let value_to_string = value_to_string

  let rec search_in_ctx (name : string) ctx =
    match (StringMap.find_opt name ctx.locals, ctx.parent) with
    | None, None ->
        stackframe_to_string ctx |> print_endline;
        print_endline name;
        failwith @@ "Value not found: " ^ name
    | Some x, _ -> x
    | None, Some p -> search_in_ctx name p


  let rec ctx_contains (name : string) ctx =
    match (StringMap.find_opt name ctx.locals, ctx.parent) with
    | None, None -> false
    | Some _, _ -> true
    | None, Some p -> ctx_contains name p


  let rec all_but_root ctx =
    let merge _ a _ = Some a in
    match (ctx.parent, ctx.parent |> Option.map (fun x -> x.parent)) with
    | _, None -> StringMap.empty
    | Some p, _ -> StringMap.union merge ctx.locals (all_but_root p)
    | _ -> StringMap.empty


  let initial_stack = { parent = None; locals = Builtins.builtins }

  let eval_literal (e : Ast.literal) =
    match e with
    | IntLiteral x -> VInt x
    | StringLiteral x -> VString (CCUtf8_string.of_string_exn x)
    | BoolLiteral x -> VBool x
    | UnitLiteral -> VUnit
    | FloatLiteral x -> VFloat x
    | CharLiteral x -> VChar x


  let rec set_pattern (p : Ast.pattern) (e : value) vars =
    match (fst p, e) with
    | PatEmptyList, VList [] -> Ok vars
    | PatListCons (ph, pt), VList (vh :: vt) ->
        set_pattern ph vh vars >>= fun vars' -> set_pattern pt (VList vt) vars'
    | PatWildcard, _ -> Ok vars
    | PatUnit, VUnit -> Ok vars
    | PatUnit, VLazy _ -> Ok vars
    | PatVariable name, value -> Ok (StringMap.add name value vars)
    | PatTuple ps, VTuple vs ->
        let zipped = List.combine ps vs in
        let folder vars (k, v) = vars >>= fun vars -> set_pattern k v vars in
        List.fold_left folder (Ok vars) zipped
    | PatCtor (pname, ppat), VVariant v when pname = v.tag ->
        set_pattern ppat v.value vars
    | PatLiteral x, y when eval_literal x = y -> Ok vars
    | _ ->
        Fmt.error "Bad pattern match for %s and %s" (value_to_string e)
          (PPAst.pp_pattern_ p)


  let set_pattern_force p e v =
    match set_pattern p e v with Ok ok -> ok | Error err -> failwith err


  let set_pattern_to_ctx (p : Ast.pattern) (e : value) ctx =
    let locals = set_pattern_force p e ctx.locals in
    { ctx with locals }


  let rec has_module name ctx =
    match (StringMap.find_opt name ctx.locals, ctx.parent) with
    | Some (VModule _), _ -> true
    | _, None -> false
    | _, Some p -> has_module name p


  let step_counter = ref 0
  let logging_offset = ref ""

  let step_num () =
    step_counter := !step_counter + 1;
    !step_counter


  let rec eval_expr' (e : Ast.expr) ctx =
    (* Fmt.pr "   %s" (stackframe_to_string ctx);
    read_line () |> ignore; *)
    match fst e with
    | Const lit -> eval_literal lit
    | Application (callee, arg) ->
        let callee' = eval_expr callee ctx in
        let arg' = eval_expr arg ctx in
        eval_application callee' arg' ctx
    | Value name -> search_in_ctx name ctx
    | LetIn (true, (PatVariable name, p), expr, body) ->
        let lazy_value = VLazy (expr, all_but_root ctx) in
        let ctx' = set_pattern_to_ctx (PatVariable name, p) lazy_value ctx in
        let expr' = eval_expr expr ctx' in
        let ctx'' = set_pattern_to_ctx (PatVariable name, p) expr' ctx' in
        eval_expr body ctx''
    | LetIn (isrec, pat, expr, body) ->
        if isrec then failwith "This expr cannot be recursive"
        else
          let expr' = eval_expr expr ctx in
          let ctx' = set_pattern_to_ctx pat expr' ctx in
          eval_expr body ctx'
    | Lambda lam -> VClosure { f = lam; captured = all_but_root ctx }
    | IfThenElse ite -> eval_ite ite ctx
    | TupleInit xs ->
        let xs' = List.map (fun e -> eval_expr e ctx) xs in
        VTuple xs'
    | Ctor c when has_module c ctx -> search_in_ctx c ctx
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
    | RecordUpdate (v, updates) ->
        let v' = eval_expr v ctx in
        let updates' =
          List.map (fun (name, e) -> (name, eval_expr e ctx)) updates
        in
        update_record v' updates' ctx
    | EmptyList -> VList []


  and eval_expr e ctx =
    let prev = !logging_offset in
    Fmt.pr "   %s-> %s\n" !logging_offset (PPAst.pp_expr e);
    logging_offset := !logging_offset ^ "| ";
    let res = eval_expr' e ctx in
    logging_offset := prev;
    Fmt.pr "   %s<- %s\n" !logging_offset (value_to_string res);
    res


  and update_record (v : value) (updates : (string * value) list) _ctx =
    match v with
    | VRecord r ->
        List.fold_left (fun m (k, v) -> StringMap.add k v m) r updates
        |> fun x -> VRecord x
    | e ->
        let msg = Fmt.str "Cannot update non-record: %s" (value_to_string e) in
        failwith msg


  and eval_match (scrutinee : value) branches ctx =
    let try_branch (branch : Ast.match_pattern_branch) =
      match set_pattern branch.pattern scrutinee ctx.locals with
      | Ok locals ->
          let ctx' = { ctx with locals } in
          let t = (Ast.Const (BoolLiteral true), Ast.Unknown) in
          let when_clause = Option.value ~default:t branch.when_clause in
          let guard_result = eval_expr when_clause ctx' in
          if guard_result = VBool true then Ok (eval_expr branch.result ctx')
          else Error "Guard failed"
      | Error e -> Error e
    in
    match branches with
    | [] ->
        let msg = value_to_string scrutinee in
        Fmt.failwith "Non exhaustive match %s" msg
    | h :: t ->
      (match try_branch h with
      | Ok v -> v
      | Error _ -> eval_match scrutinee t ctx)


  and eval_ite (ite : Ast.ite_body) ctx =
    let cond' = eval_expr ite.cond ctx in
    match cond' with
    | VBool true -> eval_expr ite.thenBranch ctx
    | VBool false -> eval_expr ite.elseBranch ctx
    | _ -> failwith "Awaited bool"


  and eval_field_access (target : value) (field : string) _ =
    let find x m =
      match StringMap.find_opt x m with
      | Some x -> x
      | None -> Fmt.failwith "Key %s was not found" x
    in
    match target with
    | VRecord r -> find field r
    | VModule m -> find field m.values
    | e ->
        let v = value_to_string e in
        Fmt.failwith "Can only lookup value in record or module. Got: %s" v


  and unlazy v ctx =
    let merge _ _ a = Some a in
    match v with
    | VLazy (x, cap) ->
        eval_expr x { ctx with locals = StringMap.union merge ctx.locals cap }
    | e -> e


  and eval_application (callee : value) (arg : value) ctx =
    match unlazy callee ctx with
    | VBuiltin b -> b arg
    | VClosure closure ->
      (match fst closure.f.body with
      | Lambda lam ->
          let cap =
            set_pattern_force (fst closure.f.arg) arg closure.captured
          in
          VClosure { f = lam; captured = cap }
      | _ ->
          let merger _ _ b = Some b in
          let locals' = StringMap.union merger ctx.locals closure.captured in
          let ctx' = { ctx with locals = locals' } in
          let ctx'' = set_pattern_to_ctx (fst closure.f.arg) arg ctx' in
          eval_expr closure.f.body ctx'')
    | VVariant v when v.value = VUnit -> VVariant { v with value = arg }
    | e ->
        Fmt.pr "%s to %s" (value_to_string e) (value_to_string arg);
        Fmt.failwith "Cannot apply non-function: %s" (value_to_string e)


  let interpret_decl ctx (d : cc_decl) =
    let fun_wrapping acc arg = (Ast.Lambda { arg; body = acc }, Ast.Unknown) in
    match d with
    | CCLetGroup decls ->
        let add_decl_to_ctx ctx (d : cc_decl_data) =
          let v = List.fold_left fun_wrapping d.expr (List.rev d.args) in
          let lazy_value = VLazy (v, StringMap.empty) in
          set_pattern_to_ctx d.pat lazy_value ctx
        in
        let eval_decl ctx (d : cc_decl_data) =
          let v = List.fold_left fun_wrapping d.expr (List.rev d.args) in
          let body' = eval_expr v ctx in
          set_pattern_to_ctx d.pat body' ctx
        in
        let ctx' = List.fold_left add_decl_to_ctx ctx decls in
        List.fold_left eval_decl ctx' decls
    | CCLet d ->
        let v = List.fold_left fun_wrapping d.expr (List.rev d.args) in
        let value' = eval_expr v ctx in
        set_pattern_to_ctx d.pat value' ctx


  let interpret (p : cc_decl list) =
    List.fold_left interpret_decl initial_stack p
end
