open Parsing.Ast.Ast

module SkbDesugar = struct
  let desugar_list_cons_pat pat =
    let rec inner' pat =
      match pat with
      | PatUnit -> pat
      | PatVariable _ -> pat
      | PatTuple vs -> PatTuple (List.map inner vs)
      | PatCtor (c, n) -> PatCtor (c, inner n)
      | PatWildcard -> pat
      | PatListCons (l, r) ->
          PatCtor ("Конк$", (PatTuple [ inner l; inner r ], Unknown))
      | PatEmptyList -> PatCtor ("Нил$", (PatUnit, Unknown))
      | PatLiteral _ -> pat
    and inner pat = (inner' (fst pat), snd pat) in
    inner pat


  let desugar_expr (expr : expr) =
    let rec inner' expr =
      match expr with
      | TupleInit vs -> TupleInit (List.map inner vs)
      | Const _ -> expr
      | Value _ -> expr
      | LetIn (r, p, i, e) ->
          LetIn (r, desugar_list_cons_pat p, inner i, inner e)
      | Lambda lm ->
          let arg = (desugar_list_cons_pat (fst lm.arg), snd lm.arg) in
          Lambda { arg; body = inner lm.body }
      | IfThenElse ite ->
          let cond = inner ite.cond in
          let thenBranch = inner ite.thenBranch in
          let elseBranch = inner ite.elseBranch in
          IfThenElse { cond; thenBranch; elseBranch }
      | Application (l, r) -> Application (inner l, inner r)
      | Ctor _ -> expr
      | RecordInit _ -> failwith "TODO"
      | RecordUpdate _ -> failwith "TODO"
      | FieldAccess _ -> failwith "TODO"
      | Match (scr, brs) ->
          let scr = inner scr in
          let desugar_branch (br : match_pattern_branch) =
            let pattern = desugar_list_cons_pat br.pattern in
            let when_clause = Option.map inner br.when_clause in
            let result = inner br.result in
            { pattern; when_clause; result }
          in
          let brs = List.map desugar_branch brs in
          Match (scr, brs)
      | EmptyList ->
          Application ((Ctor "Нил$", Unknown), (Const UnitLiteral, Unknown))
    and inner expr = (inner' (fst expr), snd expr) in
    inner expr


  let desugar_let (l : let_decl) =
    let body = desugar_expr l.body in
    let name = desugar_list_cons_pat l.name in
    { l with body; name }


  let desugar_decl (decl : decl) =
    match decl with
    | LetDecl l -> LetDecl (desugar_let l)
    | LetDeclRecursiveGroup ls ->
        LetDeclRecursiveGroup (List.map desugar_let ls)
    | ModuleDecl _ -> failwith "TODO"
    | _ -> decl


  let desugar (program : program) = List.map desugar_decl program
end
