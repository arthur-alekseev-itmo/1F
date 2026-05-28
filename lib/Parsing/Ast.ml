module Ast = struct
  type pos = Lexing.position
  type range = Known of pos * pos | Unknown | Eof

  type literal =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | BoolLiteral of bool
    | CharLiteral of Uchar.t
    | UnitLiteral

  type typ_ground =
    | TypUnit
    | TypChar
    | TypString
    | TypInt
    | TypBool
    | TypFloat

  type typ_content =
    | TypGround of typ_ground
    | TypVar of string
    | TypArrow of typ * typ
    | TypTuple of typ list
    | TypCtor of string * typ list

  and typ = typ_content * range

  type pattern_content =
    | PatUnit
    | PatVariable of string
    | PatTuple of pattern list
    | PatCtor of string * pattern
    | PatWildcard
    | PatListCons of pattern * pattern
    | PatLiteral of literal
    | PatEmptyList

  and pattern = pattern_content * range

  type typed_pattern = pattern * typ

  type ite_body = { cond: expr; thenBranch: expr; elseBranch: expr }
  and lambda_body = { arg: typed_pattern; body: expr }

  and expr_content =
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

  and expr = expr_content * range

  and match_pattern_branch =
    { pattern: pattern; when_clause: expr option; result: expr }

  type let_decl = { name: pattern; body: expr; typ: typ }
  type adt_ctor_decl = { ctor_name: string; typ: typ option }
  type rcd_field_decl = { field_name: string; typ: typ }
  type generic_var = string * range

  type decl =
    | LetDeclRecursiveGroup of let_decl list
    | LetDecl of let_decl
    | ModuleDecl of { name: string; decls: decl list }
    | AliasDecl of string * generic_var list * typ
    | AdtDecl of string * generic_var list * adt_ctor_decl list
    | RecordDecl of string * generic_var list * rcd_field_decl list

  type program = decl list
end
