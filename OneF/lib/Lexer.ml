module Lexer = struct
  type lexeme_payload =
    | IntLiteral of string
    | FloatLiteral of string
    | StringLiteral of string
    | BoolLiteral of string
    | Let | RecFlag | In
    | Operator of string
    | Identifier of string
    | Wildcard
    | Fun
    | If | Then | Else
    | LPar | RPar
    | LBr | RBr
    | LCbr | RCbr
    | Eof
    | Type
    | VBar
    | Of
    | Semicolon
    | Dot | Comma

  type lexeme_position = {
    line: int;
    start: int;
    length: int;
    file: string;
  }

  type lexeme = {
    payload: lexeme_payload;
    position: lexeme_position;
  }
end


