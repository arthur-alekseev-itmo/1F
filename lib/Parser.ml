open Lexemes.Lexemes
open Ast.Ast

module Parser = struct
  type token = t
  type input = token list

  (* Parsing results *)
  type 'a parse_result =
    | Failed of string
    | Parsed of 'a * input
    | HardFailed of string

  type 'a parser = input -> 'a parse_result

  (* return parser *)
  let return x : _ parser = fun s -> Parsed (x, s)
  let fail message _ = Failed message
  let hardfail message _ = HardFailed message

  (* Parse token if cond returns true *)
  let parse_token cond = function
    | h :: t when cond h -> return h t
    | h :: _ ->
        Failed (Format.asprintf "symbol \"%s\" not resolved" (to_string h))
    | _ -> Failed "unexpected EOF"

  let token t = parse_token (( = ) t)

  let ( >>= ) p f s =
    match p s with
    | Failed msg -> Failed msg
    | HardFailed msg -> HardFailed msg
    | Parsed (h, t) -> f h t

  let ( let* ) = ( >>= )
  let ( *> ) p1 p2 = p1 >>= fun _ -> p2
  let ( <* ) p1 p2 = p1 >>= fun h -> p2 *> return h
  let ( >> ) p1 p2 s = match p1 s with Parsed (_, t) -> p2 t | _ -> p2 s

  (* Makes parser fail with hardfail to give info about certain problem*)
  let ( !! ) par s =
    match par s with
    | Parsed (h, t) -> Parsed (h, t)
    | Failed m | HardFailed m -> HardFailed m

  (* Makes parser fail with a specific error *)
  let ( -/-> ) par msg s =
    match par s with
    | Parsed (h, t) -> Parsed (h, t)
    | HardFailed m -> HardFailed m
    | Failed _ -> Failed msg

  (* or operator *)
  let ( <|> ) p1 p2 s =
    match p1 s with
    | Failed _ -> p2 s
    | HardFailed msg -> HardFailed msg
    | res -> res

  let ( << ) p1 p2 s = (p1 <* p2 <|> p1) s

  (* if the parser fails will return None, else ruturs Some 'a *)
  let wrap p i =
    match p i with
    | Parsed (h, t) -> return (Some h) t
    | Failed _ | HardFailed _ -> return None i

  let fail_if_parsed p inp =
    match p inp with
    | Parsed (_, _) -> Failed "success"
    | Failed _ | HardFailed _ -> return () inp

  (* Parses many that are parsed by parser given *)
  let rec some v =
    let* x = v in
    let* other = many v in
    return @@ (x :: other)

  and many v = some v <|> return []

  let sep_by ~inner_parser ~sep_parser =
    many (inner_parser <* sep_parser)
    >>= (fun others -> inner_parser >>= fun last -> return (others @ [ last ]))
    <|> return []

  let rec fix f x = f (fix f) x

  (************ Domain ************)

  let lift2 f a b =
    let* a' = a in
    let* b' = b in
    f a' b'

  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= go

  let parens p = token LPar *> p <* token RPar

  let lN_operator starts : (expr -> expr -> expr parser) parser =
    let continue op a b =
      return @@ Application (Application (Value op, a), b)
    in
    let check_prefix x =
      List.for_all (fun c -> String.starts_with ~prefix:c x |> not) starts
      |> not
    in
    let inner op =
      match op with
      | Operator x when check_prefix x -> return @@ continue x
      | _ -> fail "Not an operator"
    in
    let* token = parse_token (fun _ -> true) in
    inner token

  let parse_id =
    let* t = parse_token (fun _ -> true) in
    match t with
    | SmallIdentifier i -> return @@ i
    | _ -> fail "Not an identifier"

  let parse_big_id =
    let* t = parse_token (fun _ -> true) in
    match t with
    | BigIdentifier i -> return @@ i
    | _ -> fail "Not an identifier"

  let parse_value =
    let* id = parse_id in
    return @@ Value id

  let parse_ctor =
    let* name = parse_big_id in
    return @@ Ctor name

  let parse_numeric =
    let* t = parse_token (fun _ -> true) in
    match t with
    | IntLiteral x -> return @@ Const (IntLiteral x)
    | FloatLiteral x -> return @@ Const (FloatLiteral x)
    | StringLiteral x -> return @@ Const (StringLiteral x)
    | BoolLiteral x -> return @@ Const (BoolLiteral x)
    | _ -> fail "Not a literal"

  let parse_operator_literal =
    let* token = parens (parse_token (fun _ -> true)) in
    match token with Operator x -> return x | _ -> fail "Not an operator"

  let rec parse_pattern input =
    let inner =
      let* atom = parse_pattern_atom in
      let* conss = many @@ (token (Operator "::") *> parse_pattern_atom) in
      return @@ List.fold_left (fun acc x -> PatListCons (acc, x)) atom conss
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
    (pat_wild <|> just_id <|> operator_id <|> ctor_pattern <|> others) input

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
      let* args = some parse_atom_or_access in
      return @@ List.fold_left (fun c a -> Application (c, a)) callee args
    in
    inner input

  and parse_atom_or_access input = parse_field_access input

  and parse_record_init input =
    let field_assignment =
      let* field = parse_id in
      let* value = token (Operator "=") *> parse_expr in
      return (field, value)
    in
    let inner =
      let semi = token Semicolon in
      let content = sep_by ~inner_parser:field_assignment ~sep_parser:semi in
      let* result = token LCbr *> content <* token RCbr in
      return @@ RecordInit result
    in
    inner input

  and parse_field_access input =
    let inner =
      let* atom = parse_atom in
      let dot = token Dot in
      let others = sep_by ~inner_parser:parse_id ~sep_parser:dot in
      let* fields = token Dot *> others in
      return @@ List.fold_left (fun acc x -> FieldAccess (acc, x)) atom fields
    in
    (inner <|> parse_atom) input

  and parse_list_construction input =
    let inner =
      let semi = token Semicolon in
      let cons a b = Application (Application (Value "::", b), a) in
      let elements = sep_by ~inner_parser:parse_expr ~sep_parser:semi in
      let* res = token LBr *> elements <* token RBr in
      return @@ List.fold_left cons EmptyList (List.rev res)
    in
    inner input

  and parse_atom input =
    (parse_list_construction <|> parse_lambda <|> parse_match <|> parse_ctor
   <|> parse_ite <|> parse_operator_value <|> parse_tuple <|> parse_value
   <|> parse_numeric <|> parse_let <|> parse_record_init)
      input

  and parse_let input =
    let inner =
      let* _skip = token Let in
      let* recursive = wrap (token Rec) in
      let* pat = parse_pattern in
      let* args = many parse_pattern in
      let* value = token (Operator "=") *> parse_expr in
      let* body = token In *> parse_expr in
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
      let* cond = token If *> parse_expr in
      let* thenBranch = token Then *> parse_expr in
      let* elseBranch = token Else *> parse_expr in
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
      let* _skip = token Lambda in
      let* args = some parse_pattern in
      let* body = token Arrow *> parse_expr in
      return @@ List.fold_left (fun body arg -> Lambda { body; arg }) body args
    in
    inner input

  and parse_match input =
    let inner =
      let* scrutinee = token Match *> parse_expr <* token With in
      let* first_branch = (wrap @@ token VBar) *> match_branch in
      let* other_branches = many @@ (token VBar *> match_branch) in
      let branches = first_branch :: other_branches in
      return @@ Match (scrutinee, branches)
    in
    inner input

  and parse_expr input =
    let l1_expr = parse_application <|> parse_field_access in
    let operators =
      [
        lN_operator [ "!"; "~" ];
        lN_operator [ "#" ];
        (* TODO: These ones above are higher than application! *)
        lN_operator [ "*"; "/" ];
        lN_operator [ "+"; "-" ];
        lN_operator [ ":" ]; (* TODO: Right assoc*)
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

  let expr_of_string s =
    Lexer.Lexer.lex_string s
    |> Result.map (fun r -> List.map (fun (x, _, _) -> x) r)
    |> Result.map (fun x -> parse_expr x)
end
