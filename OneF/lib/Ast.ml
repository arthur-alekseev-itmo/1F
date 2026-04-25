module Ast = struct
  type pattern = PatUnit | PatVariable of string | PatTuple of pattern list

  type literal =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | BoolLiteral of bool
    | UnitLiteral

  type ite_body = { cond : expr; thenBranch : expr; elseBranch : expr }
  and lambda_body = { arg : pattern; body : expr }

  and expr =
    | TupleInit of expr list
    | Const of literal
    | Value of string
    | LetIn of bool * pattern * expr * expr
    | Lambda of lambda_body
    | IfThenElse of ite_body
    | Application of expr * expr

  and decl = { name : pattern; recursive : bool; body : expr }

  type program = decl list
end
