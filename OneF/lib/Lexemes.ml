module Lexemes = struct
  type t = 
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | BoolLiteral of bool 
    | Let
    | Rec
    | In
    | Arrow
    | Equals
    | Operator of string
    | Wildcard
    | Identifier of string
    | Lambda 
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
    | Unit
  [@@deriving show]
end
