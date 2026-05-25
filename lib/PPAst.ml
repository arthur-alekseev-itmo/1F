open Ast.Ast
open Fmt

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

  let rec pp_ground_ (ground : typ_ground) =
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

  let rec pp_expr_ (force_par : bool) (offset : string) (expr : expr) =
    let pp_expr_' = pp_expr_ false in
    match fst expr with
    | TupleInit vs ->
        let values = List.map (pp_expr_' offset) vs in
        String.concat ", " values |> Fmt.str "(%s)"
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
    | IfThenElse ite -> Fmt.str "ITE TODO"
    | Application (f, arg) ->
        let f = pp_expr_' offset f in
        let arg = pp_expr_ true offset arg in
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
    Fmt.str "| %s%s -> %s" pat_s when_s expr_s

  let pp_expr (expr : expr) = pp_expr_ false "" expr
end
