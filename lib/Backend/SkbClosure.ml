open Parsing.Ast.Ast
open Parsing.PPAst

module SkbClosure = struct
  module StringSet = Set.Make (String)

  let union_all = List.fold_left StringSet.union StringSet.empty
  let diff_with b a = StringSet.diff a b
  let current_id = ref 0

  let global_names =
    let translation_fns =
      [ "напечатай"; "считай"; "дебаг"; "список_из_строки"; "строка_из_списка";
        "число_из_символа"; "символ_из_числа"; "число_из_строки";
        "строка_из_числа"; "прочитай_файл"; "не" ]
    in
    let ops = [ "+"; "-"; "*"; "/"; "%"; "^"; "@"; "&&"; "||" ] in
    let cmps = [ "<"; ">"; "="; "<>"; ">="; "<="; "::" ] in
    StringSet.of_list (translation_fns @ cmps @ ops)


  let fresh_name name =
    current_id := !current_id + 1;
    Fmt.str "%s$%d" name !current_id


  let rec get_pattern_vars (pat : pattern) =
    match fst pat with
    | PatUnit -> StringSet.empty
    | PatVariable v -> StringSet.singleton v
    | PatTuple vs -> union_all @@ List.map get_pattern_vars vs
    | PatCtor (_, args) -> get_pattern_vars args
    | PatWildcard -> StringSet.empty
    | PatListCons (h, t) ->
        StringSet.union (get_pattern_vars h) (get_pattern_vars t)
    | PatLiteral _ -> StringSet.empty
    | PatEmptyList -> StringSet.empty


  let get_ty_pattern_vars pat = get_pattern_vars @@ fst pat

  type cc_decl_data = { pat : pattern; args : typed_pattern list; expr : expr }
  type cc_decl = CCLet of cc_decl_data | CCLetGroup of cc_decl_data list

  let rec pp_cc_decl = function
    | CCLet l ->
        let p = PPAst.pp_pattern_ l.pat in
        let args = l.args |> List.map PPAst.pp_t_pat_ |> String.concat " " in
        let expr = PPAst.pp_expr l.expr in
        Fmt.str "clet %s %s = %s" p args expr
    | CCLetGroup l ->
        List.map (fun i -> CCLet i) l
        |> List.map pp_cc_decl
        |> String.concat "\n"


  let rec free_vars (expr : expr) =
    match fst expr with
    | TupleInit vs -> List.map free_vars vs |> union_all
    | Const _ -> StringSet.empty
    | Value v -> StringSet.singleton v
    | LetIn (r, pat, e, in_e) ->
        let self_vars = get_pattern_vars pat in
        let in_e = free_vars in_e |> StringSet.diff self_vars in
        let e =
          if r then free_vars e |> StringSet.diff self_vars else free_vars e
        in
        StringSet.union in_e e
    | Lambda lam ->
        let e = free_vars lam.body in
        let bound = get_pattern_vars (fst lam.arg) in
        StringSet.diff e bound
    | IfThenElse ite ->
        let c = free_vars ite.cond in
        let t = free_vars ite.thenBranch in
        let e = free_vars ite.elseBranch in
        union_all [ c; t; e ]
    | Application (f, a) -> StringSet.union (free_vars f) (free_vars a)
    | Ctor _ -> StringSet.empty
    | RecordInit flds -> union_all (List.map snd flds |> List.map free_vars)
    | RecordUpdate (e, flds) ->
        let e = free_vars e in
        union_all (e :: (List.map snd flds |> List.map free_vars))
    | FieldAccess (e, _) -> free_vars e
    | Match (e, brs) ->
        let e = free_vars e in
        let brs = List.map free_vars_br brs in
        StringSet.union e @@ union_all brs
    | EmptyList -> StringSet.empty


  and free_vars_br br =
    let e = free_vars br.result in
    let w =
      Option.map free_vars br.when_clause
      |> Option.value ~default:StringSet.empty
    in
    let p = get_pattern_vars br.pattern in
    StringSet.diff (StringSet.union e w) p


  let rec fold_lambdas (expr : expr) =
    match fst expr with
    | Lambda lam ->
        let e, args = fold_lambdas lam.body in
        (e, lam.arg :: args)
    | _ -> (expr, [])


  let rec rename_var old_name new_name (expr : expr) =
    let rename_var' = rename_var old_name new_name in
    match fst expr with
    | TupleInit vs -> (TupleInit (List.map rename_var' vs), snd expr)
    | Const _ -> expr
    | Value v when v = old_name -> (Value new_name, snd expr)
    | Value _ -> expr
    | LetIn (r, pat, oexpr, in_expr) ->
        let variables = get_pattern_vars pat in
        let should_rename_expr = r && not (StringSet.mem old_name variables) in
        let oexpr = if should_rename_expr then rename_var' oexpr else oexpr in
        let should_rename_inexpr = not (StringSet.mem old_name variables) in
        let in_expr =
          if should_rename_inexpr then rename_var' in_expr else in_expr
        in
        (LetIn (r, pat, oexpr, in_expr), snd expr)
    | Lambda lam ->
        let should_rename =
          get_pattern_vars (fst lam.arg) |> StringSet.mem old_name |> not
        in
        let body = if should_rename then rename_var' lam.body else lam.body in
        (Lambda { lam with body }, snd expr)
    | IfThenElse ite ->
        let cond = ite.cond |> rename_var' in
        let thenBranch = ite.thenBranch |> rename_var' in
        let elseBranch = ite.elseBranch |> rename_var' in
        (IfThenElse { cond; thenBranch; elseBranch }, snd expr)
    | Application (l, r) ->
        (Application (rename_var' l, rename_var' r), snd expr)
    | Ctor _ -> expr
    | RecordInit _ -> failwith "TODO"
    | RecordUpdate _ -> failwith "TODO"
    | FieldAccess _ -> failwith "TODO"
    | Match (scr, brs) ->
        let rename_in_branch (br : match_pattern_branch) =
          let should =
            br.pattern |> get_pattern_vars |> StringSet.mem old_name |> not
          in
          let when_clause =
            if should then (Option.map rename_var') br.when_clause
            else br.when_clause
          in
          let result = if should then rename_var' br.result else br.result in
          { br with result; when_clause }
        in
        (Match (rename_var' scr, List.map rename_in_branch brs), snd expr)
    | EmptyList -> expr


  let rec cc_expr (globals : StringSet.t) (expr : expr) :
      expr * cc_decl_data list =
    let cc_expr' = cc_expr globals in
    match fst expr with
    | TupleInit vs ->
        let converted = List.map cc_expr' vs in
        let additional = List.concat_map snd converted in
        let exprs = List.map fst converted in
        let e = (TupleInit exprs, snd expr) in
        (e, additional)
    | Const _ -> (expr, [])
    | Value _ -> (expr, [])
    | LetIn (_, (PatVariable orig_name, pos1), (Lambda lam, pos2), in_expr) ->
        let name = fresh_name orig_name in
        let globals_with_self = StringSet.add name globals in
        let bnd_expr =
          Lambda { lam with body = rename_var orig_name name lam.body }
        in
        let self, add1 = cc_lambdas globals_with_self (bnd_expr, pos2) name in
        let in_expr, add2 = cc_expr' in_expr in
        let e = LetIn (false, (PatVariable orig_name, pos1), self, in_expr) in
        ((e, snd expr), add1 @ add2)
    | LetIn (r, pat, expr, in_expr) ->
        let expr, add1 = cc_expr' expr in
        let in_expr, add2 = cc_expr' in_expr in
        let e = LetIn (r, pat, expr, in_expr) in
        ((e, snd expr), add1 @ add2)
    | Lambda _ ->
        let name = fresh_name "lam" in
        cc_lambdas globals expr name
    | IfThenElse ite ->
        let cond, add1 = cc_expr' ite.cond in
        let thenBranch, add2 = cc_expr' ite.thenBranch in
        let elseBranch, add3 = cc_expr' ite.elseBranch in
        ( (IfThenElse { cond; thenBranch; elseBranch }, snd expr),
          add1 @ add2 @ add3 )
    | Application (f, a) ->
        let f, add1 = cc_expr' f in
        let a, add2 = cc_expr' a in
        ((Application (f, a), snd expr), add1 @ add2)
    | Ctor _ -> (expr, [])
    | RecordInit _ -> failwith "TODO"
    | RecordUpdate _ -> failwith "TODO"
    | FieldAccess _ -> failwith "TODO"
    | Match (scrutinee, brs) ->
        let scr, add1 = cc_expr' scrutinee in
        let cc_branch br =
          let when_clause, add1 =
            Option.map cc_expr' br.when_clause
            |> Option.map (fun (a, b) -> (Some a, b))
            |> Option.value ~default:(None, [])
          in
          let result, add2 = cc_expr' br.result in
          ({ pattern = br.pattern; when_clause; result }, add1 @ add2)
        in
        let mapped = List.map cc_branch brs in
        let brs = List.map fst mapped in
        let adds = List.concat_map snd mapped in
        ((Match (scr, brs), snd expr), add1 @ adds)
    | EmptyList -> (expr, [])


  and cc_lambdas (globals : StringSet.t) lams name =
    let cc_expr' = cc_expr globals in
    let lambda_body, lambda_args = fold_lambdas lams in
    let expr, inners = cc_expr' lambda_body in
    let lambda_arg_names = List.map get_ty_pattern_vars lambda_args in
    let captured_values =
      free_vars expr
      |> diff_with globals
      |> diff_with (union_all lambda_arg_names)
      |> StringSet.to_list
    in
    let unk_ty = (TypVar "?", Unknown) in
    let env_args =
      List.map (fun v -> ((PatVariable v, Unknown), unk_ty)) captured_values
    in
    let args = env_args @ lambda_args in
    let cc_decl = { pat = (PatVariable name, snd expr); args; expr } in
    let apply acc arg = (Application (acc, (Value arg, Unknown)), Unknown) in
    let applied_expr =
      List.fold_left apply (Value name, Unknown) captured_values
    in
    (applied_expr, cc_decl :: inners)


  let globals = ref global_names

  let cc_let (decl : let_decl) =
    let body, args = fold_lambdas decl.body in
    let body, additional = cc_expr !globals body in
    let cc_decl = { pat = decl.name; args; expr = body } in
    additional @ [ cc_decl ]


  let add_pattern_to_globals (pattern : pattern) =
    let bound = get_pattern_vars pattern in
    globals := !globals |> StringSet.union bound


  let cc_decl (decl : decl) =
    match decl with
    | LetDeclRecursiveGroup decls ->
        let names = List.map (fun x -> x.name) decls in
        List.iter add_pattern_to_globals names;
        [ CCLetGroup (List.concat_map cc_let decls) ]
    | LetDecl decl ->
        add_pattern_to_globals decl.name;
        [ CCLetGroup (cc_let decl) ]
    | ModuleDecl _ -> failwith "TODO: CC Module"
    | _ -> []


  let convert_closures (program : program) =
    let res = List.concat_map cc_decl program in
    List.iter (fun s -> print_endline @@ pp_cc_decl s) res;
    res


  let convert_closures_in_expr (expr : expr) =
    let e, decls = cc_expr !globals expr in
    (e, [ CCLetGroup decls ])
end
