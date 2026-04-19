module Lexemes = struct
  type lexeme_payload =
    | IntLiteral of string
    | FloatLiteral of string
    | StringLiteral of string
    | BoolLiteral of string
    | Let
    | RecFlag
    | In
    | Operator of string
    | Wildcard
    | Identifier of string
    | Fun
    | Type
    | Eof
    | LCbr
    | RCbr
    | LBr
    | RBr
    | LPar
    | RPar
    | If
    | Then
    | Else
    | VBar
    | Semicolon
    | Of
    | Dot
    | Comma
  [@@deriving show]

  type lexeme_position = { line : int; start : int; length : int; file : string } [@@deriving show]
  type lexeme = { payload : lexeme_payload; position : lexeme_position } [@@deriving show]
  type t = lexeme list

  let dump_file (_path : string) (_lexemes : t) = failwith "TODO"
end
