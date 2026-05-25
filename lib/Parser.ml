open Lexemes.Lexemes
open Ast.Ast

module Parser = struct
  type pos = Lexing.position
  type range = Known of pos * pos | Unknown | Eof
  type token = t * pos * pos
  type input = token list

  (* Parsing results *)
  type 'a parse_result =
    | Failed of string * range
    | Parsed of 'a * input
    | HardFailed of string * range

  type 'a parser = input -> 'a parse_result

  (* return parser *)
  let return x : _ parser = fun s -> Parsed (x, s)
  let fail message ps pe _ = Failed (message, Known (ps, pe))
  let just_fail message _ = Failed (message, Unknown)
  let hardfail message ps pe _ = HardFailed (message, Known (ps, pe))
  let just_hardfail message _ = HardFailed (message, Unknown)

  (* Parse token if cond returns true *)
  let parse_token cond = function
    | (h, ps, pe) :: t when cond h -> return (h, ps, pe) t
    | (h, ps, pe) :: t ->
        fail (Format.asprintf "Token '%s' not resolved" (to_string h)) ps pe t
    | _ -> just_fail "unexpected EOF" []

  let token t = parse_token (( = ) t)

  let ( >>= ) p f s =
    match p s with
    | Failed (msg, range) -> Failed (msg, range)
    | HardFailed (msg, range) -> HardFailed (msg, range)
    | Parsed (h, t) -> f h t

  let ( let* ) = ( >>= )
  let ( *> ) p1 p2 = p1 >>= fun _ -> p2
  let ( <* ) p1 p2 = p1 >>= fun h -> p2 *> return h
  let ( >> ) p1 p2 s = match p1 s with Parsed (_, t) -> p2 t | _ -> p2 s

  (* or operator *)
  let ( <|> ) p1 p2 s =
    match p1 s with
    | Failed _ -> p2 s
    | HardFailed (msg, range) -> HardFailed (msg, range)
    | res -> res

  let ( << ) p1 p2 s = (p1 <* p2 <|> p1) s

  let must (p : 'a parser) (msg : string) : 'a parser =
   fun input ->
    match p input with
    | Failed (_, range) -> HardFailed (msg, range)
    | other -> other

  let must_pos ((ps, pe) : pos * pos) (p : 'a parser) (msg : string) : 'a parser
      =
   fun input ->
    match p input with
    | Failed (_, _) -> HardFailed (msg, Known (ps, pe))
    | other -> other

  (* if the parser fails will return None, else ruturs Some 'a *)
  let wrap p i =
    match p i with
    | Parsed (h, t) -> return (Some h) t
    | Failed _ | HardFailed _ -> return None i

  let fail_if_parsed p inp =
    match p inp with
    | Parsed (_, _) -> just_fail "success" inp
    | Failed _ | HardFailed _ -> return () inp

  (* Parses many that are parsed by parser given *)
  let rec some v =
    let* x = v in
    let* other = many v in
    return @@ (x :: other)

  and many v = some v <|> return []

  let sep_by ~inner_parser ~sep_parser =
    let* first = wrap @@ inner_parser in
    match first with
    | Some first ->
        let* other = many @@ (sep_parser *> inner_parser) in
        return @@ (first :: other)
    | None -> return []

  let rec fix f x = f (fix f) x

  let must_token t =
    let* tok = wrap @@ parse_token (fun _ -> true) in
    match tok with
    | Some (tok, ps, pe) when t = tok -> return (tok, ps, pe)
    | Some (tok, ps, pe) ->
        let s_tok = to_string tok in
        let s_t = to_string t in
        let msg = Format.sprintf "Awaited: '%s', but got: '%s'" s_t s_tok in
        hardfail msg ps pe
    | None ->
        let msg =
          Format.sprintf "Unexpected EOF, but awaited: '%s'" (to_string t)
        in
        fun _ -> HardFailed (msg, Eof)

  let between t_start t_end content =
    let* _, ps, pse = token t_start in
    let* content = content in
    let* tok_end = wrap @@ parse_token (fun _ -> true) in
    match tok_end with
    | Some (t, _, _) when t = t_end -> return content
    | Some (x, _, pe) ->
        let s_t = to_string t_end in
        let s_x = to_string x in
        let msg =
          Format.sprintf "Unmatched brackets: got '%s' instead of '%s'" s_x s_t
        in
        hardfail msg ps pe
    | None -> hardfail "Bracket is unmached: EOF" ps pse

  (************ Domain ************)

  let lift2 f a b =
    let* a' = a in
    let* b' = b in
    f a' b'

  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= go

  let parens p = between LPar RPar p

  let lN_operator starts : (expr -> expr -> expr parser) parser =
    let continue op a b =
      return @@ Application (Application (Value op, a), b)
    in
    let check_prefix x =
      List.for_all (fun c -> String.starts_with ~prefix:c x |> not) starts
      |> not
    in
    let inner (op, ps, pe) =
      match op with
      | Operator x when check_prefix x -> return @@ continue x
      | _ -> fail "Not an operator" ps pe
    in
    let* token = parse_token (fun _ -> true) in
    inner token

  let parse_id =
    let* t, ps, pe = parse_token (fun _ -> true) in
    match t with
    | SmallIdentifier i -> return @@ i
    | _ -> fail "Not an identifier" ps pe

  let parse_big_id =
    let* t, ps, pe = parse_token (fun _ -> true) in
    match t with
    | BigIdentifier i -> return @@ i
    | _ -> fail "Not an identifier" ps pe

  let parse_value =
    let* id = parse_id in
    return @@ Value id

  let parse_ctor =
    let* name = parse_big_id in
    return @@ Ctor name

  let parse_literal =
    let* t, ps, pe = parse_token (fun _ -> true) in
    match t with
    | IntLiteral x -> return (IntLiteral x)
    | FloatLiteral x -> return (FloatLiteral x)
    | StringLiteral x -> return (StringLiteral x)
    | BoolLiteral x -> return (BoolLiteral x)
    | CharLiteral x -> return (CharLiteral x)
    | _ -> fail "Not a literal" ps pe

  let parse_numeric =
    let* lit = parse_literal in
    return @@ Const lit

  let parse_operator_literal =
    let* token, ps, pe =
      token LPar *> parse_token (fun _ -> true) <* token RPar
    in
    match token with
    | Operator x -> return x
    | _ -> fail "Not an operator" ps pe

  let parse_ground =
    let* t = parse_id in
    let ret x = TypGround x |> return in
    match t with
    | "инт" -> ret TypInt
    | "бул" -> ret TypBool
    | "скиб" -> ret TypUnit
    | "строка" -> ret TypString
    | "дроб" -> ret TypFloat
    | other -> return (TypVar other)

  let rec parse_ty input =
    let inner =
      let* atoms =
        sep_by ~inner_parser:parse_ty_atom ~sep_parser:(token Arrow)
      in
      match List.rev atoms with
      | [] -> just_fail "No type atoms parsed"
      | h :: t -> return @@ List.fold_left (fun acc b -> TypArrow (b, acc)) h t
    in
    inner input

  and parse_ty_ctor input =
    let inner =
      let* name = parse_big_id in
      let parse_args =
        let comma = token Comma in
        let p = sep_by ~inner_parser:parse_ty ~sep_parser:comma in
        between (Operator "<") (Operator ">") p
      in
      let* raw_args = wrap parse_args in
      let args = Option.value ~default:[] raw_args in
      return @@ TypCtor (name, List.rev args)
    in
    inner input

  and parse_ty_tuple input =
    let inner =
      let content =
        let comma = token Comma in
        sep_by ~sep_parser:comma ~inner_parser:parse_ty
      in
      let* res = between LPar RPar content in
      match res with
      | [] -> return @@ TypGround TypUnit
      | [ x ] -> return x
      | lst -> return @@ TypTuple lst
    in
    inner input

  and parse_ty_atom input =
    (parse_ground <|> parse_ty_ctor <|> parse_ty_tuple) input

  let rec parse_pattern input =
    let inner =
      let* atom = parse_pattern_atom in
      let* conss = many @@ (token (Operator "::") *> parse_pattern_atom) in
      return @@ List.fold_left (fun acc x -> PatListCons (acc, x)) atom conss
    in
    inner input

  and parse_typed_pattern input =
    let inner =
      let content =
        let* pat = parse_pattern in
        let* ty = must_token Colon *> parse_ty in
        return (pat, ty)
      in
      between LPar RPar content
    in
    inner input

  and parse_pattern_atom input =
    let operator_id =
      let* lit = parse_operator_literal in
      return @@ PatVariable lit
    in
    let pat_wild = token Wildcard *> return PatWildcard in
    let ctor_pattern =
      let* ctor_name = parse_big_id in
      let* pat_option = wrap parse_pattern in
      let unit = PatUnit in
      let pat = Option.value ~default:unit pat_option in
      return @@ PatCtor (ctor_name, pat)
    in
    let pat_empty_list = token LBr *> return PatEmptyList <* token RBr in
    let pat_literal = parse_literal >>= fun n -> return @@ PatLiteral n in
    let just_id = parse_id >>= fun id -> return @@ PatVariable id in
    let others =
      let* in_parens =
        parens @@ sep_by ~inner_parser:parse_pattern ~sep_parser:(token Comma)
      in
      match in_parens with
      | [] -> return PatUnit
      | [ x ] -> return x
      | xs -> return @@ PatTuple xs
    in
    (pat_literal <|> pat_empty_list <|> pat_wild <|> just_id <|> operator_id
   <|> ctor_pattern <|> others)
      input

  let parse_operator_value =
    parse_operator_literal >>= fun v -> return @@ Value v

  let rec parse_tuple input =
    let inner =
      let* in_parens =
        parens @@ sep_by ~inner_parser:parse_expr ~sep_parser:(token Comma)
      in
      match in_parens with
      | [] -> return @@ Const UnitLiteral
      | [ x ] -> return x
      | xs -> return @@ TupleInit xs
    in
    inner input

  and parse_application input =
    let inner =
      let* callee = parse_atom_or_access in
      let* args = many parse_atom_or_access in
      return @@ List.fold_left (fun c a -> Application (c, a)) callee args
    in
    inner input

  and parse_atom_or_access input = parse_field_access input

  and field_assignment input =
    let inner =
      let* field = parse_id in
      let* value = token (Operator "=") *> parse_expr in
      return (field, value)
    in
    inner input

  and parse_record_update input =
    let inner =
      let semi = token Semicolon in
      let content =
        let* target = parse_expr <* token With in
        let* updates = sep_by ~inner_parser:field_assignment ~sep_parser:semi in
        return (target, updates)
      in
      let* target, updates = token LCbr *> content <* must_token RCbr in
      return @@ RecordUpdate (target, updates)
    in
    inner input

  and parse_record_init input =
    let inner =
      let semi = token Semicolon in
      let content = sep_by ~inner_parser:field_assignment ~sep_parser:semi in
      let* result = between LCbr RCbr content in
      return @@ RecordInit result
    in
    inner input

  and parse_field_access input =
    let inner =
      let* atom = parse_atom in
      let dot = token Dot in
      let others = sep_by ~inner_parser:parse_id ~sep_parser:dot in
      let* fields = wrap @@ (token Dot *> others) in
      let fields = Option.value ~default:[] fields in
      return @@ List.fold_left (fun acc x -> FieldAccess (acc, x)) atom fields
    in
    inner input

  and parse_list_construction input =
    let inner =
      let semi = token Semicolon in
      let cons a b = Application (Application (Value "::", b), a) in
      let elements = sep_by ~inner_parser:parse_expr ~sep_parser:semi in
      let* res = between LBr RBr elements in
      return @@ List.fold_left cons EmptyList (List.rev res)
    in
    inner input

  and parse_atom input =
    (parse_list_construction <|> parse_lambda <|> parse_match <|> parse_ctor
   <|> parse_ite <|> parse_operator_value <|> parse_tuple <|> parse_value
   <|> parse_numeric <|> parse_let <|> parse_record_update <|> parse_record_init
    )
      input

  and parse_let input =
    let inner =
      let* _, lps, lpe = token Let in
      let* recursive = wrap (token Rec) in
      let* pat = must_pos (lps, lpe) parse_pattern "Awaited pattern" in
      let* args = many parse_typed_pattern in
      let* _maybe_ty = wrap @@ (token Colon *> parse_ty) in
      let eq = must_token (Operator "=") in
      let* value = must (eq *> parse_expr) "Awaited expr after 'пусть'" in
      let* _, ips, ipe = token In in
      let* body = must_pos (ips, ipe) parse_expr "Awaited expr after 'в'" in
      let fun_expr =
        List.fold_left
          (fun body arg -> Lambda { arg; body })
          value (List.rev args)
      in
      return @@ LetIn (recursive |> Option.is_some, pat, fun_expr, body)
    in
    inner input

  and parse_ite input =
    let inner =
      let* _, ips, ipe = token If in
      let* cond = must_pos (ips, ipe) parse_expr "Awaited expr after 'если'" in
      let* _, tps, tpe = must_token Then in
      let* thenBranch =
        must_pos (tps, tpe) parse_expr "Awaited expr after 'то'"
      in
      let* _, eps, epe = must_token Else in
      let* elseBranch =
        must_pos (eps, epe) parse_expr "Awaited expr after 'иначе'"
      in
      return @@ IfThenElse { cond; thenBranch; elseBranch }
    in
    inner input

  and match_branch input =
    let inner =
      let* pattern = parse_pattern in
      let* when_clause = wrap (token When *> parse_expr) in
      let* result = token Arrow *> parse_expr in
      return @@ { pattern; when_clause; result }
    in
    inner input

  and parse_lambda input =
    let inner =
      let* _, lps, lpe = token Lambda in
      let parse_args =
        must_pos (lps, lpe) (some parse_typed_pattern)
          "Awaited at least one pattern"
      in
      let* args = parse_args in
      let* _, aps, ape = must_token Arrow in
      let* body =
        must_pos (aps, ape) parse_expr "Awaited expr in lambda body"
      in
      return @@ List.fold_left (fun body arg -> Lambda { body; arg }) body args
    in
    inner input

  and parse_match input =
    let inner =
      let* scrutinee = token Match *> parse_expr <* must_token With in
      let* first_branch = (wrap @@ token VBar) *> match_branch in
      let* other_branches = many @@ (token VBar *> match_branch) in
      let branches = first_branch :: other_branches in
      return @@ Match (scrutinee, branches)
    in
    inner input

  and parse_expr input =
    let l1_expr = parse_application in
    let operators =
      [
        lN_operator [ "!"; "~" ];
        lN_operator [ "#" ];
        (* TODO: These ones above are higher than application! *)
        lN_operator [ "*"; "/"; "%" ];
        lN_operator [ "+"; "-" ];
        lN_operator [ ":" ];
        (* TODO: Right assoc *)
        lN_operator [ "^"; "@" ];
        lN_operator [ "<"; ">"; "=" ];
        lN_operator [ "&" ];
        lN_operator [ "|" ];
        lN_operator [ "," ];
        lN_operator [ ";" ];
      ]
    in
    let parser = List.fold_left chainl1 l1_expr operators in
    parser input

  let parse_typ_decl input =
    let parse_generics =
      let comma = token Comma in
      let gens = sep_by ~inner_parser:parse_id ~sep_parser:comma in
      let* wrapped = wrap (between (Operator "<") (Operator ">") gens) in
      return @@ Option.value ~default:[] wrapped
    in
    let parse_record_type name =
      let* generics = parse_generics in
      let* _ = must_token (Operator "=") in
      let field_decl =
        let* id = parse_id in
        let* typ = must_token Colon *> parse_ty in
        return { field_name = id; typ }
      in
      let comma = token Comma in
      let* data =
        between LCbr RCbr (sep_by ~inner_parser:field_decl ~sep_parser:comma)
      in
      return @@ RecordDecl (name, generics, data)
    in
    let parse_adt_type name =
      let* generics = parse_generics in
      let* _ = must_token (Operator "=") in
      let variant_decl =
        let* id = parse_big_id in
        let* typ = wrap (token Of *> parse_ty) in
        return { ctor_name = id; typ }
      in
      let msg = "Awaited variant declaration" in
      let* first_variant = (wrap @@ token VBar) *> must variant_decl msg in
      let next_variants = token VBar *> must variant_decl msg in
      let* others = many next_variants in
      return @@ AdtDecl (name, generics, first_variant :: others)
    in
    let inner =
      let* _ = token Type in
      let* name = must parse_big_id "Type must have a name" in
      parse_record_type name <|> parse_adt_type name
    in
    inner input

  let rec parse_decl input =
    (parse_module <|> parse_let_decl <|> parse_typ_decl) input

  and parse_module input =
    let inner =
      let* _ = token Module in
      let* name = must parse_big_id "Module must have a name (capitalized)" in
      let* _ = must_token (Operator "=") in
      let* _ = must_token Struct in
      let* decls = many parse_decl in
      let* _ = must_token End in
      return @@ ModuleDecl { name; decls }
    in
    inner input

  and parse_let_decl input =
    let parse_binding_after_let (lps, lpe) : let_decl parser =
      let* name =
        must_pos (lps, lpe) parse_pattern "Let binding must have a name"
      in
      let* args = many parse_typed_pattern in
      let* typ = must_token Colon *> parse_ty in
      let* _, ps, pe = must_token (Operator "=") in
      let* raw_body = must_pos (ps, pe) parse_expr "Awaited expr after eq" in
      let body =
        List.fold_left
          (fun body arg -> Lambda { arg; body })
          raw_body (List.rev args)
      in
      let arg_typs = List.map snd args |> List.rev in
      let typ = List.fold_left (fun b a -> TypArrow (a, b)) typ arg_typs in
      return { name; body; typ }
    in
    let inner =
      let* _, lps, lpe = token Let in
      let* rec_f = wrap (token Rec) in
      let recursive = Option.is_some rec_f in
      match recursive with
      | true ->
          let* main_block = parse_binding_after_let (lps, lpe) in
          let and_blocks =
            token And >>= fun (_, lps, lpe) -> parse_binding_after_let (lps, lpe)
          in
          let* other = many and_blocks in
          return @@ LetDeclRecursiveGroup (main_block :: other)
      | false ->
          let* main_block = parse_binding_after_let (lps, lpe) in
          return @@ LetDecl main_block
    in
    inner input

  let parse_program = many parse_decl
  let expr_of_string s = Lexer.Lexer.lex_string s |> Result.map parse_program
  let program_of_string s = Lexer.Lexer.lex_string s |> Result.map parse_program
end
