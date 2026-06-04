open Angstrom
open Ast.Ast

module AngstromParser = struct
  let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
  let is_id_byte c = is_alpha c || is_digit c || c >= '\x80'
  let is_id_start c = is_alpha c || c >= '\x80'

  let is_op_char = function
    | '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<'
    | '=' | '>' | '?' | '@' | '^' | '|' | '~' | ';' -> true
    | _ -> false

  let is_op_char_strict = function
    | '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<'
    | '=' | '>' | '?' | '@' | '^' | '~' | ';' -> true
    | _ -> false

  let ws = skip_while is_ws
  let optional p = option None (p >>| fun x -> Some x)

  let operator_with_pipe = take_while1 is_op_char <* ws
  let operator_no_pipe = take_while1 is_op_char_strict <* ws

  let kw s =
    string s *> peek_char >>= function
    | Some c when is_id_byte c -> fail "keyword boundary"
    | _ -> ws *> return s

  let is_capital s =
    if s = "" then false
    else
      let fc = String.get_utf_8_uchar s 0 in
      Uchar.utf_decode_is_valid fc
      && Uucp.Gc.general_category (Uchar.utf_decode_uchar fc) = `Lu

  let keywords =
    ["пусть"; "рек"; "и"; "модуль"; "структура"; "конец"; "тип"; "алиас";
     "из"; "да"; "нет"; "инт"; "бул"; "скиб"; "строка"; "символ"; "дроб";
     "в"; "если"; "то"; "иначе"; "сопоставить"; "с";
     "лямбда"; "когда"; "открыть"]

  let ident =
    take_while1 is_id_start >>= fun first ->
    take_while is_id_byte >>= fun rest ->
    ws *> return (first ^ rest)

  let any_id = ident

  let small_id = ident >>= fun s ->
    if is_capital s then fail "expected lowercase identifier"
    else if List.mem s keywords then fail "keyword"
    else return s

  let big_id = ident >>= fun s ->
    if is_capital s then return s else fail "expected uppercase identifier"

  let expr_of_lit lit = (Const lit, Unknown)

  let float_lit =
    take_while1 is_digit >>= fun whole ->
    (char '.' *> take_while is_digit >>= fun frac ->
     ws *> return (expr_of_lit (FloatLiteral (float_of_string (whole ^ "." ^ frac)))))
    <|> (ws *> return (expr_of_lit (IntLiteral (int_of_string whole))))

  let char_lit =
    char '\'' *>
    ((satisfy (fun c -> c <> '\'' && c <> '\\') >>= fun c ->
      char '\'' *> ws *> return (expr_of_lit (CharLiteral (Uchar.of_char c))))
     <|> (char '\\' *> satisfy (fun _ -> true) >>= fun esc ->
           char '\'' *> ws *> return (expr_of_lit (CharLiteral (Uchar.of_char
             (match esc with
              | 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r'
              | '\\' -> '\\' | '\'' -> '\'' | '"' -> '"'
              | c -> c)))))
     <|> (char '\'' *> ws *> return (expr_of_lit (CharLiteral (Uchar.of_char '\'')))))

  let string_lit =
    char '"' *>
    take_while (fun c -> c <> '"' && c <> '\\') >>= fun s ->
    many (char '\\' *> satisfy (fun _ -> true) >>= fun esc ->
          take_while (fun c -> c <> '"' && c <> '\\') >>= fun rest ->
          return (String.make 1
            (match esc with
             | 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r'
             | '\\' -> '\\' | '"' -> '"' | '\'' -> '\''
             | c -> c) ^ rest))
    >>= fun parts ->
    char '"' *> ws *> return (expr_of_lit (StringLiteral (s ^ String.concat "" parts)))
    <|> fail "expected string literal"

  let bool_lit =
    (kw "да" *> return (expr_of_lit (BoolLiteral true)))
    <|> (kw "нет" *> return (expr_of_lit (BoolLiteral false)))

  let literal = float_lit <|> bool_lit <|> string_lit <|> char_lit

  let chainl1 e op =
    let rec go acc =
      (op >>= fun f -> e >>= fun x -> go (f acc x)) <|> return acc
    in
    e >>= go

  let parse_pattern_atom =
    fix (fun self ->
      let wildcard = kw "_" *> return (PatWildcard, Unknown) in
      let pat_ctor_or_var =
        any_id >>= fun name ->
        if is_capital name then
          optional self >>= fun arg ->
          let arg = Option.value ~default:(PatUnit, Unknown) arg in
          return (PatCtor (name, arg), Unknown)
        else if List.mem name keywords then fail "keyword in pattern"
        else
          return (PatVariable name, Unknown)
      in
      let pat_literal =
        float_lit >>= fun lit ->
        match lit with
        | (Const (IntLiteral n), _) -> return (PatLiteral (IntLiteral n), Unknown)
        | (Const (FloatLiteral f), _) -> return (PatLiteral (FloatLiteral f), Unknown)
        | (Const (StringLiteral s), _) -> return (PatLiteral (StringLiteral s), Unknown)
        | (Const (BoolLiteral b), _) -> return (PatLiteral (BoolLiteral b), Unknown)
        | (Const (CharLiteral c), _) -> return (PatLiteral (CharLiteral c), Unknown)
        | _ -> fail "not a literal in pattern"
      in
      let empty_list =
        char '[' *> ws *> char ']' *> ws *> return (PatEmptyList, Unknown)
      in
      let pat_parens =
        let open_paren = char '(' *> ws in
        let unit_pat = char ')' *> ws *> return (PatUnit, Unknown) in
        let op_pat = peek_char >>= function
          | Some c when is_op_char c ->
              operator_with_pipe <* char ')' <* ws >>= fun op ->
              return (PatVariable op, Unknown)
          | _ -> fail "not op"
        in
        let tuple_or_pat =
          self >>= fun first ->
          (char ',' *> ws *>
             sep_by1 (char ',' <* ws) self >>= fun rest ->
             char ')' *> ws *> return (PatTuple (first :: rest), Unknown))
          <|> (char ')' *> ws *> return first)
        in
        open_paren *> (unit_pat <|> op_pat <|> tuple_or_pat)
      in
      wildcard <|> pat_literal <|> pat_ctor_or_var <|> empty_list <|> pat_parens
    )

  let parse_pattern_cons =
    parse_pattern_atom >>= fun atom ->
    many (kw "::" *> parse_pattern_atom) >>= fun conss ->
    return
    @@ List.fold_left
         (fun acc (x, _) -> (PatListCons (acc, (x, Unknown)), Unknown))
         atom conss

  let parse_pattern = parse_pattern_cons

  let parse_typ_atom parse_typ =
    let typ_id =
      any_id >>= fun name ->
      if is_capital name then
        optional (char '<' *> ws *> sep_by1 (char ',' <* ws) parse_typ <* char '>' <* ws)
        >>| fun args -> ((TypCtor (name, Option.value ~default:[] args)), Unknown)
      else
        match name with
        | "инт" -> return ((TypGround TypInt, Unknown))
        | "бул" -> return ((TypGround TypBool, Unknown))
        | "скиб" -> return ((TypGround TypUnit, Unknown))
        | "строка" -> return ((TypGround TypString, Unknown))
        | "символ" -> return ((TypGround TypChar, Unknown))
        | "дроб" -> return ((TypGround TypFloat, Unknown))
        | other -> return ((TypVar other, Unknown))
    in
    let typ_tuple =
      char '(' *> ws *>
      (char ')' *> ws *> return ((TypGround TypUnit, Unknown))
       <|> (sep_by1 (char ',' <* ws) parse_typ >>= fun ts ->
            char ')' *> ws *>
            match ts with
            | [t] -> return t
            | xs -> return ((TypTuple xs, Unknown))))
    in
    typ_tuple <|> typ_id

  let parse_typ =
    fix (fun self ->
      let atom = parse_typ_atom self in
      sep_by1 (kw "->") atom >>= fun atoms ->
      match List.rev atoms with
      | [x] -> return x
      | h :: t ->
          return
          @@ List.fold_left
               (fun acc b -> (TypArrow (b, acc), Unknown))
               h t
      | [] -> fail "empty type"
    )

  let parse_pattern_arg =
    let default_typ = (TypGround TypUnit, Unknown) in
    ws *>
    (let typed =
       char '(' *> ws *> parse_pattern >>= fun pat ->
       char ':' *> ws *> parse_typ >>= fun typ ->
       char ')' *> ws *> return ((pat, typ))
     in
     let untyped = parse_pattern >>= fun pat -> return ((pat, default_typ)) in
     typed <|> untyped)

  let parse_expr =
    fix (fun self ->
      let make_lambda_args args body =
        List.fold_right
          (fun (p, t) b -> (Lambda { arg = (p, t); body = b }, Unknown))
          args body
      in
      let op_binop op a b =
        (Application ((Application ((Value op, Unknown), a), Unknown), b), Unknown)
      in

      let cons_list es =
        List.fold_left
          (fun acc e ->
            let op = (Value "::", Unknown) in
            (Application ((Application (op, e), Unknown), acc), Unknown))
          (EmptyList, Unknown) (List.rev es)
      in

      (* Atoms *)
      let value = small_id >>| fun id -> (Value id, Unknown) in
      let ctor = big_id >>| fun name -> (Ctor name, Unknown) in
      let wildcard = kw "_" *> return (Value "_", Unknown) in

      let parse_tuple_or_parens =
        char '(' *> ws *>
        (char ')' *> ws *> return (Const UnitLiteral, Unknown)
         <|>
         (sep_by1 (char ',' <* ws) self >>= fun es ->
          char ')' *> ws *>
          match es with
          | [e] -> return e
          | xs -> return (TupleInit xs, Unknown)))
      in

      let parse_list =
        char '[' *> ws *>
        (char ']' *> ws *> return (EmptyList, Unknown)
         <|>
         (sep_by (char ';' <* ws) self >>= fun es ->
          char ']' *> ws *> return (cons_list es)))
      in

      let parse_record_init =
        char '{' *> ws *>
        sep_by (char ';' <* ws)
          (let* name = small_id in
           let* _ = kw "=" in
           let* value = self in
           return (name, value))
        <* char '}' <* ws
        >>| fun fields -> (RecordInit fields, Unknown)
      in

      let op_in_parens =
        char '(' *> ws *> operator_with_pipe <* char ')' <* ws
        >>| fun op -> (Value op, Unknown)
      in

      let parse_lambda =
        kw "лямбда" *>
        many1 parse_pattern_arg >>= fun pats ->
        kw "->" *> self >>= fun body ->
        return (make_lambda_args pats body)
      in

      let parse_atom =
        literal <|> value <|> ctor <|> wildcard
        <|> parse_tuple_or_parens <|> parse_list
        <|> parse_record_init <|> op_in_parens <|> parse_lambda
      in

      let parse_field_access =
        let* e = parse_atom in
        let* fields = many (char '.' *> ws *> small_id) in
        return
        @@ List.fold_left (fun acc f -> (FieldAccess (acc, f), Unknown)) e fields
      in

      let parse_app =
        let* callee = parse_field_access in
        let* args = many parse_field_access in
        return
        @@ List.fold_left
             (fun (c, _) a -> (Application ((c, Unknown), a), Unknown))
             callee args
      in

      (* Operators (precedence climbing) *)
      let make_lop op_fn starts =
        let first_chars = List.map (fun s -> String.get s 0) starts |> List.filter (fun c -> c <> '\000') in
        let inner =
          peek_char >>= function
          | Some c when List.mem c first_chars ->
            op_fn >>= fun op ->
            if op = "->" then fail "arrow is not an operator"
            else if List.exists (fun p -> String.starts_with ~prefix:p op) starts then
              return (fun a b -> op_binop op a b)
            else
              fail "not in operator group"
          | _ -> fail "wrong start char"
        in
        inner
      in

      let parse_operators base pipe_group op_fn =
        let ops =
          [ make_lop op_fn [ "*"; "/"; "%" ]; make_lop op_fn [ "+"; "-" ];
            make_lop op_fn [ ":" ];
            make_lop op_fn [ "^"; "@" ];
            make_lop op_fn [ "<"; ">"; "=" ];
            make_lop op_fn [ "&" ] ]
          @ pipe_group
          @ [ make_lop op_fn [ "," ]; make_lop op_fn [ ";" ] ]
        in
        List.fold_left (fun p op -> chainl1 p op) base ops
      in

      let pipe_ops =
        [peek_string 2 >>= fun s2 ->
         if String.length s2 >= 2 && Char.equal (String.get s2 0) '|' && is_op_char (String.get s2 1) then
           operator_with_pipe >>= fun op ->
           return (fun a b -> op_binop op a b)
         else
           fail "not a pipe operator"] in
      let no_pipe_ops = [] in

      (* High-level expressions *)
      let parse_if =
        kw "если" *> self >>= fun cond ->
        kw "то" *> self >>= fun then_ ->
        kw "иначе" *> self >>= fun else_ ->
        return (IfThenElse { cond; thenBranch = then_; elseBranch = else_ }, Unknown)
      in

      let parse_match =
        kw "сопоставить" *> self >>= fun e ->
        kw "с" *> optional (kw "|") *>
        sep_by1 (kw "|")
          (let* pat = parse_pattern in
            let* when_clause =
              optional (kw "когда" *> self)
            in
           let* _ = kw "->" in
           let* result = parse_operators parse_app no_pipe_ops operator_no_pipe in
           return { pattern = pat; when_clause; result })
        >>= fun branches ->
        return (Match (e, branches), Unknown)
      in

      let parse_let_in =
        kw "пусть" *>
        (let* rec_flag = optional (kw "рек") in
         let recursive = Option.is_some rec_flag in
         let* pat = parse_pattern in
         let* args = many parse_pattern_arg in
         let* _ = kw "=" in
         let* bound = self in
         let* _ = kw "в" in
         let* body = self in
         let bound = make_lambda_args args bound in
          return (LetIn (recursive, pat, bound, body), Unknown))
      in

      parse_let_in <|> parse_if <|> parse_match <|> parse_operators parse_app pipe_ops operator_with_pipe
    )

  let parse_let_decl =
    kw "пусть" *>
    (let* rec_flag = optional (kw "рек") in
     let recursive = Option.is_some rec_flag in
     let* pat = parse_pattern in
     let* args = many parse_pattern_arg in
     let* ty_ann =
       optional (char ':' *> ws *> parse_typ)
       >>| Option.value ~default:(TypGround TypUnit, Unknown)
     in
     let* _ = kw "=" in
     let* body = parse_expr in
     let fun_body =
      List.fold_right
        (fun (p, t) b -> (Lambda { arg = (p, t); body = b }, Unknown))
        args body
    in
    if recursive then
      let* others =
        many
          (kw "и" *>
            (let* pat2 = parse_pattern in
             let* args2 = many parse_pattern_arg in
             let* ty2 =
               optional (char ':' *> ws *> parse_typ)
               >>| Option.value ~default:(TypGround TypUnit, Unknown)
             in
             let* _ = kw "=" in
             let* body2 = parse_expr in
             let fun_body2 =
               List.fold_right
                 (fun (p, t) b -> (Lambda { arg = (p, t); body = b }, Unknown))
                 args2 body2
             in
             return { name = pat2; body = fun_body2; typ = ty2 }))
      in
      return
        (LetDeclRecursiveGroup ({ name = pat; body = fun_body; typ = ty_ann } :: others))
    else
      return (LetDecl { name = pat; body = fun_body; typ = ty_ann }))

  let parse_adt_variant =
    let* ctor_name = big_id in
    let* typ = optional (kw "из" *> parse_typ) in
    return { ctor_name; typ }

  let parse_record_field =
    let* field_name = small_id in
    let* _ = char ':' <* ws in
    let* typ = parse_typ in
    return { field_name; typ }

  let parse_typ_decl =
    let generics =
      optional
        (char '<' *> ws *> sep_by1 (char ',' <* ws) small_id <* char '>' <* ws)
      >>| Option.value ~default:[]
    in
    let gens_list gens =
      List.map (fun s -> (s, Unknown)) gens
    in
    let adt_decl name gens =
      optional (kw "|")
      *> sep_by1 (kw "|") parse_adt_variant >>= fun variants ->
      return (AdtDecl (name, gens_list gens, variants))
    in
    let record_decl name gens =
      char '{' *> ws
      *> sep_by (char ';' <* ws) parse_record_field
      <* char '}' <* ws >>= fun fields ->
      return (RecordDecl (name, gens_list gens, fields))
    in
    let alias_decl name gens =
      parse_typ >>= fun t ->
      return (AliasDecl (name, gens_list gens, t))
    in
    (kw "тип" *> big_id >>= fun name ->
     generics >>= fun gens ->
     kw "=" *> (record_decl name gens <|> adt_decl name gens))
    <|> (kw "алиас" *> big_id >>= fun name ->
         generics >>= fun gens -> kw "=" *> alias_decl name gens)

  let parse_module_decl parse_decl =
    kw "модуль" *> big_id >>= fun name ->
    kw "=" *> kw "структура" *> many parse_decl <* kw "конец"
    >>= fun decls -> return (ModuleDecl { name; decls })

  let parse_decl =
    fix (fun self ->
      ws *> (parse_let_decl <|> parse_module_decl self <|> parse_typ_decl)
    )

  let parse_program =
    ws *> many parse_decl <* end_of_input

  let parse_expr_only =
    ws *> parse_expr <* end_of_input

  let program_of_string (input : string) : (program, string) result =
    match parse_string ~consume:All parse_program input with
    | Ok prog -> Ok prog
    | Error e -> Error e

  let expr_of_string (input : string) : (expr, string) result =
    match parse_string ~consume:All parse_expr_only input with
    | Ok e -> Ok e
    | Error e -> Error e
end
