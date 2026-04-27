module Ast = struct
  type pattern =
    | PatUnit
    | PatVariable of string
    | PatTuple of pattern list
    | PatCtor of string * pattern
    | PatWildcard
    | PatListCons of pattern * pattern
    | PatEmptyList

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
    | Ctor of string
    | RecordInit of (string * expr) list
    | RecordUpdate of expr * (string * expr) list
    | FieldAccess of expr * string
    | Match of expr * match_pattern_branch list
    | EmptyList

  and match_pattern_branch = {
    pattern : pattern;
    when_clause : expr option;
    result : expr;
  }

  and decl = { name : pattern; recursive : bool; body : expr }

  type program = decl list
end
