open Lexemes

module Lexer = struct
  include Lexemes

  let digit = [%sedlex.regexp? '0' .. '9']
  let alpha = [%sedlex.regexp? alphabetic | '_']
  let id = [%sedlex.regexp? alpha, Star (alpha | digit)]
  let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
  let op_char = [%sedlex.regexp?  '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' ]
  let operator_reg =[%sedlex.regexp? Plus op_char] 
  let float_reg = [%sedlex.regexp? (Plus digit, '.', Star digit) | (Star digit, '.', Plus digit)]

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
    | operator_reg -> Operator (Sedlexing.Utf8.lexeme buf)
    | float_reg -> FloatLiteral (float_of_string (Sedlexing.Utf8.lexeme buf))  
    | Plus digit -> IntLiteral (int_of_string (Sedlexing.Utf8.lexeme buf))
    | '"', Star (Sub (any, '"')), '"' ->
        let s = Sedlexing.Utf8.lexeme buf in
        StringLiteral (String.sub s 1 (String.length s - 2))
    | id -> get_id (Sedlexing.Utf8.lexeme buf)
    | "->" -> Arrow
    | "=" -> Equals
    | "(" -> LPar
    | ")" -> RPar
    | "," -> Comma
    | "()" -> Unit
    | eof -> Eof
    | _ ->
        let char = Sedlexing.Utf8.lexeme buf in
        failwith ("Непонятный символ: " ^ char)

  let tokenize_incremental buf = Sedlexing.with_tokenizer token buf
end
