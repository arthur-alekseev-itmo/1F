open Lexemes

module Lexer = struct
  include Lexemes

  let ok = Result.ok
  let error = Result.error
  let ( let* ) = Result.bind
  let digit = [%sedlex.regexp? '0' .. '9']
  let alpha = [%sedlex.regexp? alphabetic | '_']
  let id = [%sedlex.regexp? alpha, Star (alpha | digit)]
  let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]

  let op_char =
    [%sedlex.regexp?
      ( '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?'
      | '@' | '^' | '|' | '~' )]

  let operator_reg = [%sedlex.regexp? Plus op_char]

  let float_reg =
    [%sedlex.regexp? Plus digit, '.', Star digit | Star digit, '.', Plus digit]

  let get_id s =
    match s with
    | "пусть" -> Let
    | "рек" -> Rec
    | "в" -> In
    | "если" -> If
    | "то" -> Then
    | "иначе" -> Else
    | "лямбда" -> Lambda
    | "да" -> BoolLiteral true
    | "нет" -> BoolLiteral false
    | s -> Identifier s

  let rec token buf =
    match%sedlex buf with
    | whitespace -> token buf
    | "->" -> ok Arrow
    | '"', Star (Sub (any, '"')), '"' ->
        let s = Sedlexing.Utf8.lexeme buf in
        ok @@ StringLiteral (String.sub s 1 (String.length s - 2))
    | "(" -> ok LPar
    | ")" -> ok RPar
    | "," -> ok Comma
    | float_reg -> ok @@ FloatLiteral (float_of_string (Sedlexing.Utf8.lexeme buf))
    | Plus digit -> ok @@ IntLiteral (int_of_string (Sedlexing.Utf8.lexeme buf))
    | operator_reg -> ok @@ Operator (Sedlexing.Utf8.lexeme buf)
    | id -> ok @@ get_id (Sedlexing.Utf8.lexeme buf)
    | eof -> ok Eof
    | _ ->
        let char = Sedlexing.Utf8.lexeme buf in
        error ("Непонятный символ: " ^ char)

  let tokenize_incremental buf = Sedlexing.with_tokenizer token buf

  let token_gen_of_string (input : string) =
    let lexbuf = Sedlexing.Utf8.from_string input in
    tokenize_incremental lexbuf

  let read_file path =
    try In_channel.with_open_text path In_channel.input_all |> Result.ok
    with Sys_error e -> Format.sprintf "Error while reading file: %s" e |> Result.error

  let rec list_of_gen (s : unit -> (t * Lexing.position * Lexing.position, string) result)
      =
    let* r = s () in
    match r with
    | Eof, _, _ -> ok []
    | triple ->
        let* tail = list_of_gen s in
        ok @@ (triple :: tail)

  let wrap_fn s i =
    let result, x, y = s i in
    match result with Result.Ok z -> ok (z, x, y) | Result.Error e -> error e

  let lex_string (content : string) =
    let tokens = wrap_fn @@ token_gen_of_string content in
    list_of_gen tokens

  let lex_file (path : string) =
    let* content = read_file path in
    lex_string content
end
