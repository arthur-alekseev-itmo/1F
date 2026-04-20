module ParseTree = struct
  type pattern = PatUnit | PatVariable of string | PatTuple of pattern list
  type literal = IntLiteral of int | StringLiteral of string | BoolLiteral of bool | UnitLiteral

  type ite_body = { cond : expr; thenBranch : expr; elseBranch : expr }
  and lambda_body = { args : pattern list; body : expr }
  and expr = Const of literal | Value of string | LetIn of decl | Lambda of lambda_body | IfThenElse of ite_body
  and decl = { name : pattern; recursive : bool; args : pattern list; body : expr }

  type program = decl list
end
