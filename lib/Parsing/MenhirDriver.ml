open Lexemes
open MenhirParser

let lexemes_to_token : Lexemes.t -> token = function
  | Type            -> TYPE
  | Of              -> OF
  | IntLiteral n    -> INT n
  | FloatLiteral f  -> FLOAT f
  | StringLiteral s -> STRING s
  | BoolLiteral b   -> BOOL b
  | CharLiteral c   -> CHAR c
  | Let             -> LET
  | Rec             -> REC
  | In              -> IN
  | Lambda          -> LAMBDA
  | Arrow           -> ARROW
  | If              -> IF
  | Then            -> THEN
  | Else            -> ELSE
  | Match           -> MATCH
  | With            -> WITH
  | When            -> WHEN
  | Wildcard        -> WILDCARD
  | LPar            -> LPAR
  | RPar            -> RPAR
  | LBr             -> LBR
  | RBr             -> RBR
  | LCbr            -> LCBR
  | RCbr            -> RCBR
  | Comma           -> COMMA
  | Semicolon       -> SEMICOLON
  | Dot             -> DOT
  | VBar            -> VBAR
  | Module          -> MODULE
  | Struct          -> STRUCT
  | End             -> END
  | Open            -> OPEN
  | TypeAlias       -> TYPEALIAS
  | And             -> AND
  | Colon           -> COLON
  | SmallIdentifier s -> SMALL_ID s
  | BigIdentifier s -> BIG_ID s
  | Eof             -> EOF
  | Operator s when String.starts_with ~prefix:"!" s
                 || String.starts_with ~prefix:"~" s
                 || String.starts_with ~prefix:"#" s -> OP_PREFIX s
  | Operator s when String.starts_with ~prefix:"*" s
                 || String.starts_with ~prefix:"/" s
                 || String.starts_with ~prefix:"%" s -> OP_MUL_DIV s
  | Operator s when String.starts_with ~prefix:"+" s
                 || String.starts_with ~prefix:"-" s -> OP_ADD_SUB s
   | Operator "::" -> OP_CONS "::"
   | Operator s when String.starts_with ~prefix:":" s -> OP_COLON s
  | Operator s when String.starts_with ~prefix:"^" s
                 || String.starts_with ~prefix:"@" s -> OP_CONS s
   | Operator "=" -> EQUAL "="
   | Operator "<" -> LESS "<"
   | Operator ">" -> GREATER ">"
   | Operator s when String.starts_with ~prefix:"<" s
                   || String.starts_with ~prefix:">" s
                   || String.starts_with ~prefix:"=" s -> OP_CMP s
  | Operator s when String.starts_with ~prefix:"&" s -> OP_AMP s
  | Operator s when String.starts_with ~prefix:"|" s -> OP_BAR s
  | Operator s when String.starts_with ~prefix:"," s -> OP_COMMA s
  | Operator s when String.starts_with ~prefix:";" s -> OP_SEMI s
  | Operator _ -> assert false


let last_error_pos : Lexing.position ref = ref Lexing.dummy_pos

let make_lexer (tokens : (token * Lexing.position * Lexing.position) list) =
  let token_list = ref tokens in
  fun (lexbuf : Lexing.lexbuf) ->
    match !token_list with
    | [] -> failwith "no more tokens"
    | (tok, s, _e) :: rest ->
        token_list := rest;
        if s != Lexing.dummy_pos then last_error_pos := s;
        lexbuf.lex_curr_p <- {
          pos_fname = lexbuf.lex_curr_p.pos_fname;
          pos_lnum = s.pos_lnum;
          pos_bol = s.pos_bol;
          pos_cnum = s.pos_cnum;
        };
        tok

let fmt_syntax_error source (pos : Lexing.position) =
  let line_num = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  let heading = Format.sprintf "File: ??, line: %d, characters %d-%d:" line_num col (col + 1) in
  let lines = String.split_on_char '\n' source in
  let content =
    try List.nth lines (line_num - 1)
    with _ -> ""
  in
  let underline = String.make col ' ' ^ "^" in
  Format.sprintf "%s\n%s\n%s\n" heading content underline

let of_tokens add_eof parser source tokens =
  let all_tokens = if add_eof
    then tokens @ [(EOF, Lexing.dummy_pos, Lexing.dummy_pos)]
    else tokens
  in
  let _ = last_error_pos := Lexing.dummy_pos in
  let lexer = make_lexer all_tokens in
  let lexbuf = Lexing.from_string "" in
  try Ok (parser lexer lexbuf)
  with MenhirParser.Error ->
    let pos = !last_error_pos in
    if pos = Lexing.dummy_pos then Error "syntax error"
    else Error (fmt_syntax_error source pos)

let parse_program (source : string) (tokens : (Lexemes.t * Lexing.position * Lexing.position) list) =
  let converted = List.map (fun (t, s, e) -> (lexemes_to_token t, s, e)) tokens in
  of_tokens true MenhirParser.program source converted

let parse_expr (source : string) (tokens : (Lexemes.t * Lexing.position * Lexing.position) list) =
  let converted = List.map (fun (t, s, e) -> (lexemes_to_token t, s, e)) tokens in
  of_tokens true MenhirParser.parse_expr source converted


  let lexemes_to_token_table : Lexemes.t -> MenhirParserTable.token = function
  | Type            -> MenhirParserTable.TYPE
  | Of              -> MenhirParserTable.OF
  | IntLiteral n    -> MenhirParserTable.INT n
  | FloatLiteral f  -> MenhirParserTable.FLOAT f
  | StringLiteral s -> MenhirParserTable.STRING s
  | BoolLiteral b   -> MenhirParserTable.BOOL b
  | CharLiteral c   -> MenhirParserTable.CHAR c
  | Let             -> MenhirParserTable.LET
  | Rec             -> MenhirParserTable.REC
  | In              -> MenhirParserTable.IN
  | Lambda          -> MenhirParserTable.LAMBDA
  | Arrow           -> MenhirParserTable.ARROW
  | If              -> MenhirParserTable.IF
  | Then            -> MenhirParserTable.THEN
  | Else            -> MenhirParserTable.ELSE
  | Match           -> MenhirParserTable.MATCH
  | With            -> MenhirParserTable.WITH
  | When            -> MenhirParserTable.WHEN
  | Wildcard        -> MenhirParserTable.WILDCARD
  | LPar            -> MenhirParserTable.LPAR
  | RPar            -> MenhirParserTable.RPAR
  | LBr             -> MenhirParserTable.LBR
  | RBr             -> MenhirParserTable.RBR
  | LCbr            -> MenhirParserTable.LCBR
  | RCbr            -> MenhirParserTable.RCBR
  | Comma           -> MenhirParserTable.COMMA
  | Semicolon       -> MenhirParserTable.SEMICOLON
  | Dot             -> MenhirParserTable.DOT
  | VBar            -> MenhirParserTable.VBAR
  | Module          -> MenhirParserTable.MODULE
  | Struct          -> MenhirParserTable.STRUCT
  | End             -> MenhirParserTable.END
  | Open            -> MenhirParserTable.OPEN
  | TypeAlias       -> MenhirParserTable.TYPEALIAS
  | And             -> MenhirParserTable.AND
  | Colon           -> MenhirParserTable.COLON
  | SmallIdentifier s -> MenhirParserTable.SMALL_ID s
  | BigIdentifier s -> MenhirParserTable.BIG_ID s
  | Eof             -> MenhirParserTable.EOF
  | Operator s when String.starts_with ~prefix:"!" s
                 || String.starts_with ~prefix:"~" s
                 || String.starts_with ~prefix:"#" s -> MenhirParserTable.OP_PREFIX s
  | Operator s when String.starts_with ~prefix:"*" s
                 || String.starts_with ~prefix:"/" s
                 || String.starts_with ~prefix:"%" s -> MenhirParserTable.OP_MUL_DIV s
  | Operator s when String.starts_with ~prefix:"+" s
                 || String.starts_with ~prefix:"-" s -> MenhirParserTable.OP_ADD_SUB s
  | Operator "::" -> MenhirParserTable.OP_CONS "::"
  | Operator s when String.starts_with ~prefix:":" s -> MenhirParserTable.OP_COLON s
  | Operator s when String.starts_with ~prefix:"^" s
                 || String.starts_with ~prefix:"@" s -> MenhirParserTable.OP_CONS s
  | Operator "=" -> MenhirParserTable.EQUAL "="
  | Operator "<" -> MenhirParserTable.LESS "<"
  | Operator ">" -> MenhirParserTable.GREATER ">"
  | Operator s when String.starts_with ~prefix:"<" s
                   || String.starts_with ~prefix:">" s
                   || String.starts_with ~prefix:"=" s -> MenhirParserTable.OP_CMP s
  | Operator s when String.starts_with ~prefix:"&" s -> MenhirParserTable.OP_AMP s
  | Operator s when String.starts_with ~prefix:"|" s -> MenhirParserTable.OP_BAR s
  | Operator s when String.starts_with ~prefix:"," s -> MenhirParserTable.OP_COMMA s
  | Operator s when String.starts_with ~prefix:";" s -> MenhirParserTable.OP_SEMI s
  | Operator _ -> assert false

let parse_program_table (source : string) (tokens : (Lexemes.t * Lexing.position * Lexing.position) list) =
  let converted = List.map (fun (t, s, e) -> (lexemes_to_token_table t, s, e)) tokens in
  let all_tokens = converted @ [(MenhirParserTable.EOF, Lexing.dummy_pos, Lexing.dummy_pos)] in
  let module P = MenhirParserTable in
  let module I = P.MenhirInterpreter in
  let token_list = ref all_tokens in
  let next_token _ =
    match !token_list with
    | [] -> P.EOF
    | (tok, _, _) :: rest ->
        token_list := rest;
        tok
  in
  let lexbuf = Lexing.from_string source in
  let supplier = I.lexer_lexbuf_to_supplier next_token lexbuf in
  match I.loop supplier (P.Incremental.program lexbuf.Lexing.lex_curr_p) with
  | prog -> Ok prog
  | exception Error -> Error "syntax error"

let parse_expr_table (source : string) (tokens : (Lexemes.t * Lexing.position * Lexing.position) list) =
  let converted = List.map (fun (t, s, e) -> (lexemes_to_token_table t, s, e)) tokens in
  let all_tokens = converted @ [(MenhirParserTable.EOF, Lexing.dummy_pos, Lexing.dummy_pos)] in
  let module P = MenhirParserTable in
  let module I = P.MenhirInterpreter in
  let token_list = ref all_tokens in
  let next_token _ =
    match !token_list with
    | [] -> P.EOF
    | (tok, _, _) :: rest ->
        token_list := rest;
        tok
  in
  let lexbuf = Lexing.from_string source in
  let supplier = I.lexer_lexbuf_to_supplier next_token lexbuf in
  match I.loop supplier (P.Incremental.parse_expr lexbuf.Lexing.lex_curr_p) with
  | expr -> Ok expr
  | exception Error -> Error "syntax error"

  
