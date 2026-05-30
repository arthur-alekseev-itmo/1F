open Ast.Ast

module PPAst = struct
  let pp_literal_ (lit : literal) =
    match lit with
    | IntLiteral i -> Fmt.str "%d" i
    | FloatLiteral f -> Fmt.str "%f" f
    | StringLiteral s -> Fmt.str "\"%s\"" s
    | BoolLiteral b -> Fmt.str "%b" b
    | CharLiteral c -> Fmt.str "'%c'" (Uchar.to_char c)
    | UnitLiteral -> "()"


  let rec pp_pattern_ (pat : pattern) =
    match fst pat with
    | PatUnit -> "()"
    | PatVariable v -> v
    | PatTuple vs ->
        List.map pp_pattern_ vs |> String.concat ", " |> Fmt.str "(%s)"
    | PatCtor (name, pat) -> Fmt.str "%s %s" name (pp_pattern_ pat)
    | PatWildcard -> "_"
    | PatListCons (h, t) -> Fmt.str "%s :: %s" (pp_pattern_ h) (pp_pattern_ t)
    | PatLiteral l -> pp_literal_ l
    | PatEmptyList -> "[]"


  let pp_ground_ (ground : typ_ground) =
    match ground with
    | TypUnit -> "скиб"
    | TypChar -> "символ"
    | TypString -> "строка"
    | TypInt -> "инт"
    | TypBool -> "бул"
    | TypFloat -> "дроб"


  let rec pp_typ_ (force_par : bool) (typ : typ) : string =
    match fst typ with
    | TypGround g -> pp_ground_ g
    | TypVar v -> v
    | TypArrow (l, r) ->
        let arr = Fmt.str "%s -> %s" (pp_typ_ true l) (pp_typ_ false r) in
        if force_par then Fmt.str "(%s)" arr else arr
    | TypTuple vs ->
        List.map (pp_typ_ false) vs |> String.concat ", " |> Fmt.str "(%s)"
    | TypCtor (c, args) ->
        let args = List.map (pp_typ_ false) args |> String.concat ", " in
        Fmt.str "%s<%s>" c args


  let pp_t_pat_ (pat, ty) =
    Fmt.str "(%s: %s)" (pp_pattern_ pat) (pp_typ_ false ty)


  let is_operator v =
    match fst v with
    | Value x when String.contains "|!@#$%^&*-=+/<>:;" x.[0] -> true
    | _ -> false


  let rec pp_expr_ (force_par : bool) (offset : string) (expr : expr) =
    let pp_expr_' = pp_expr_ false in
    match fst expr with
    | TupleInit vs ->
        let values = List.map (pp_expr_' offset) vs in
        String.concat ", " values |> Fmt.str "%s(%s)" offset
    | Const literal -> pp_literal_ literal
    | Value v -> v
    | LetIn (rc, pat, expr, in_expr) ->
        let rc_s = if rc then "рек " else "" in
        let pat_s = pp_pattern_ pat in
        let expr_s = pp_expr_' (offset ^ "  ") expr in
        let in_expr_s = pp_expr_' offset in_expr in
        Fmt.str "пусть %s%s = %s в %s" rc_s pat_s expr_s in_expr_s
    | Lambda lam ->
        Fmt.str "лм %s -> %s" (pp_t_pat_ lam.arg)
          (pp_expr_' (offset ^ "  ") lam.body)
    | IfThenElse ite ->
        Fmt.str " если %s то %s иначе %s"
          (pp_expr_' offset ite.cond)
          (pp_expr_' offset ite.thenBranch)
          (pp_expr_' offset ite.elseBranch)
    | Application (f, arg) ->
        let f' = pp_expr_' offset f in
        let arg = pp_expr_ true offset arg in
        let f, arg = if is_operator f then (arg, f') else (f', arg) in
        if force_par then Fmt.str "(%s %s)" f arg else Fmt.str "%s %s" f arg
    | Ctor c -> c
    | RecordInit _ -> "TODO: Record init"
    | RecordUpdate _ -> "TODO: Record update"
    | FieldAccess _ -> "TODO: Field access"
    | Match (scrutinee, branches) ->
        let s = pp_expr_' "" scrutinee in
        let bs = List.map (pp_branch_ offset) branches |> String.concat " " in
        Fmt.str "сопоставить %s с%s" s bs
    | EmptyList -> "[]"


  and pp_branch_ (offset : string) (br : match_pattern_branch) =
    let pat_s = pp_pattern_ br.pattern in
    let when_s =
      Option.map
        (fun s -> Fmt.str " когда %s" (pp_expr_ false "" s))
        br.when_clause
    in
    let when_s = Option.value ~default:"" when_s in
    let expr_s = pp_expr_ false offset br.result in
    Fmt.str "%s| %s%s -> %s" offset pat_s when_s expr_s


  let pp_expr (expr : expr) = pp_expr_ false "" expr

  let pp_decl (decl : decl) : string =
    let pp_gens gens =
      match gens with
      | [] -> ""
      | _ -> gens |> List.map fst |> String.concat ", " |> Fmt.str "<%s>"
    in
    let pp_variant v =
      let name = v.ctor_name in
      let args =
        match v.typ with
        | Some x -> Fmt.str " из %s" (pp_typ_ false x)
        | None -> ""
      in
      Fmt.str "| %s%s\n" name args
    in
    let pp_decl (kw : string) (decl : let_decl) =
      let t = pp_typ_ false decl.typ in
      let n = pp_pattern_ decl.name in
      let e = pp_expr_ false "  " decl.body in
      Fmt.str "%s %s : %s =\n%s\n" kw n t e
    in
    let pp_decl_group group =
      let h = List.hd group in
      let t = List.tl group in
      let h = pp_decl "пусть рек" h in
      let t = List.map (pp_decl "и") t |> String.concat "" in
      Fmt.str "%s%s" h t
    in
    match decl with
    | LetDeclRecursiveGroup decls -> pp_decl_group decls
    | LetDecl decl -> pp_decl "пусть" decl
    | ModuleDecl _ -> failwith "TODO"
    | AliasDecl (name, gens, ty) ->
        let args = pp_gens gens in
        Fmt.str "алиас %s%s = %s" name args (pp_typ_ false ty)
    | AdtDecl (name, gens, brs) ->
        let args = pp_gens gens in
        let brs = List.map pp_variant brs |> String.concat "" in
        Fmt.str "тип %s%s =%s" name args brs
    | RecordDecl (_name, _gens, _fields) -> failwith "TODO"


  let pp_program (p : program) = List.map pp_decl p |> String.concat ""
end
