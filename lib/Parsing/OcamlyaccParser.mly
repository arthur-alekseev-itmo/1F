%{
open Ast.Ast

let mrgn (startp, endp) = Known (startp, endp)

let mrg r1 r2 =
  match r1, r2 with
  | Known (a, b), Known (c, d) -> Known (min a c, max b d)
  | Unknown, e | e, Unknown -> e
  | Eof, _ | _, Eof -> Eof

let default_typ = (TypGround TypUnit, Unknown)

let ground_type_of_string s =
  match s with
  | "инт" -> Some TypInt
  | "бул" -> Some TypBool
  | "скиб" -> Some TypUnit
  | "строка" -> Some TypString
  | "символ" -> Some TypChar
  | "дроб" -> Some TypFloat
  | _ -> None

let wrap_lambda_args args body =
  List.fold_right (fun (a, a_t) b ->
      (Lambda { arg = (a, a_t); body = b }, mrg (snd a) (snd b)))
    args body

%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token <Uchar.t> CHAR
%token <string> SMALL_ID
%token <string> BIG_ID
%token <string> OP_PREFIX
%token <string> OP_MUL_DIV
%token <string> OP_ADD_SUB
%token <string> OP_COLON
%token <string> OP_CONS
%token <string> OP_CMP
%token <string> LESS
%token <string> GREATER
%token <string> EQUAL
%token <string> OP_AMP
%token <string> OP_BAR
%token <string> OP_COMMA
%token <string> OP_SEMI
%token LET REC IN
%token LAMBDA ARROW
%token IF THEN ELSE
%token MATCH WITH WHEN
%token WILDCARD
%token TYPE TYPEALIAS OF
%token LPAR RPAR LBR RBR LCBR RCBR
%token COMMA SEMICOLON DOT VBAR
%token MODULE STRUCT END OPEN
%token COLON AND
%token EOF

%nonassoc IN
%left OP_SEMI
%left OP_COMMA
%left OP_BAR
%left OP_AMP
%left OP_CMP EQUAL LESS GREATER
%right OP_CONS
%left OP_COLON
%left OP_ADD_SUB
%left OP_MUL_DIV
%nonassoc OP_PREFIX
%nonassoc prec_application
%left DOT

%start program
%type <decl list> program
%start parse_expr
%type <expr> parse_expr

%%

program:
  | decls EOF { List.rev $1 }
;

decls:
  | /* empty */ { [] }
  | decls decl { $2 :: $1 }
;

decl:
  | let_decl { $1 }
  | module_decl { $1 }
  | type_decl { $1 }
;

typed_pattern:
  | LPAR pattern COLON typ RPAR { ($2, $4) }
;

pattern_arg_list:
  | /* empty */ { [] }
  | pattern_arg_list pattern_arg { $2 :: $1 }
;

pattern_arg:
  | typed_pattern { $1 }
  | pattern { ($1, default_typ) }
;

typ:
  | typ_atom ARROW typ { (TypArrow ($1, $3), mrg (snd $1) (snd $3)) }
  | typ_atom { $1 }
;

typ_atom:
  | LPAR RPAR
      { (TypGround TypUnit, mrgn ($startpos, $endpos)) }
  | LPAR typ_tuple_body RPAR
      { match List.rev $2 with [t] -> t | xs -> (TypTuple xs, mrgn ($startpos, $endpos)) }
  | SMALL_ID
      { match ground_type_of_string $1 with
        | Some g -> (TypGround g, mrgn ($startpos, $endpos))
        | None -> (TypVar $1, mrgn ($startpos, $endpos)) }
  | BIG_ID typ_args_opt
      { let args = match $2 with Some a -> a | None -> [] in
        (TypCtor ($1, args), mrgn ($startpos, $endpos)) }
;

typ_tuple_body:
  | typ { [$1] }
  | typ_tuple_body COMMA typ { $3 :: $1 }
;

typ_args_opt:
  | /* empty */ { None }
  | LESS typ_list GREATER { Some (List.rev $2) }
;

typ_list:
  | typ { [$1] }
  | typ_list COMMA typ { $3 :: $1 }
;

let_decl:
  | LET pattern pattern_arg_list COLON typ EQUAL expr
      { LetDecl { name = $2;
                  body = wrap_lambda_args (List.rev $3) $7;
                  typ = $5 } }
  | LET pattern pattern_arg_list EQUAL expr
      { LetDecl { name = $2;
                  body = wrap_lambda_args (List.rev $3) $5;
                  typ = default_typ } }
  | LET REC pattern pattern_arg_list COLON typ EQUAL expr
      { LetDecl { name = $3;
                  body = wrap_lambda_args (List.rev $4) $8;
                  typ = $6 } }
  | LET REC pattern pattern_arg_list EQUAL expr
      { LetDecl { name = $3;
                  body = wrap_lambda_args (List.rev $4) $6;
                  typ = default_typ } }
  | LET REC pattern pattern_arg_list COLON typ EQUAL expr
    AND pattern pattern_arg_list COLON typ EQUAL expr
      { LetDeclRecursiveGroup [
          { name = $3; body = wrap_lambda_args (List.rev $4) $8;
            typ = $6 };
          { name = $10; body = wrap_lambda_args (List.rev $11) $15;
            typ = $13 }
        ] }
  | LET REC pattern pattern_arg_list EQUAL expr
    AND pattern pattern_arg_list COLON typ EQUAL expr
      { LetDeclRecursiveGroup [
          { name = $3; body = wrap_lambda_args (List.rev $4) $6;
            typ = default_typ };
          { name = $8; body = wrap_lambda_args (List.rev $9) $13;
            typ = $11 }
        ] }
  | LET REC pattern pattern_arg_list COLON typ EQUAL expr
    AND pattern pattern_arg_list EQUAL expr
      { LetDeclRecursiveGroup [
          { name = $3; body = wrap_lambda_args (List.rev $4) $8;
            typ = $6 };
          { name = $10; body = wrap_lambda_args (List.rev $11) $13;
            typ = default_typ }
        ] }
  | LET REC pattern pattern_arg_list EQUAL expr
    AND pattern pattern_arg_list EQUAL expr
      { LetDeclRecursiveGroup [
          { name = $3; body = wrap_lambda_args (List.rev $4) $6;
            typ = default_typ };
          { name = $8; body = wrap_lambda_args (List.rev $9) $11;
            typ = default_typ }
        ] }
;

module_decl:
  | MODULE BIG_ID EQUAL STRUCT decls END
      { ModuleDecl { name = $2; decls = List.rev $5 } }
;

generic_vars_opt:
  | /* empty */ { [] }
  | LESS generic_vars_list GREATER { List.rev $2 }
;

generic_vars_list:
  | SMALL_ID { [($1, mrgn ($startpos, $endpos))] }
  | generic_vars_list COMMA SMALL_ID { ($3, mrgn ($startpos, $endpos)) :: $1 }
;

type_decl:
  | TYPE BIG_ID generic_vars_opt EQUAL opt_vbar adt_variants
      { AdtDecl ($2, $3, List.rev $6) }
  | TYPE BIG_ID generic_vars_opt EQUAL LCBR record_fields RCBR
      { RecordDecl ($2, $3, List.rev $6) }
  | TYPEALIAS BIG_ID generic_vars_opt EQUAL typ
      { AliasDecl ($2, $3, $5) }
;

opt_vbar:
  | /* empty */ {}
  | VBAR {}
;

adt_variants:
  | adt_variant { [$1] }
  | adt_variants VBAR adt_variant { $3 :: $1 }
;

adt_variant:
  | BIG_ID OF typ { { ctor_name = $1; typ = Some $3 } }
  | BIG_ID { { ctor_name = $1; typ = None } }
;

record_fields:
  | /* empty */ { [] }
  | record_field { [$1] }
  | record_fields SEMICOLON record_field { $3 :: $1 }
;

record_field:
  | SMALL_ID COLON typ { { field_name = $1; typ = $3 } }
;

expr:
  | let_expr { $1 }
  | if_expr { $1 }
  | lambda_expr { $1 }
  | match_expr { $1 }
  | binop { $1 }
;

parse_expr:
  | expr EOF { $1 }
;

let_expr:
  | LET pattern pattern_arg_list COLON typ EQUAL expr IN expr
      { (LetIn (false, $2,
           wrap_lambda_args (List.rev $3) $7,
           $9),
         mrg (snd $2) (snd $9)) }
  | LET pattern pattern_arg_list EQUAL expr IN expr
      { (LetIn (false, $2,
           wrap_lambda_args (List.rev $3) $5,
           $7),
         mrg (snd $2) (snd $7)) }
  | LET REC pattern pattern_arg_list COLON typ EQUAL expr IN expr
      { (LetIn (true, $3,
           wrap_lambda_args (List.rev $4) $8,
           $10),
         mrg (snd $3) (snd $10)) }
  | LET REC pattern pattern_arg_list EQUAL expr IN expr
      { (LetIn (true, $3,
           wrap_lambda_args (List.rev $4) $6,
           $8),
         mrg (snd $3) (snd $8)) }
;

if_expr:
  | IF expr THEN expr ELSE expr
      { (IfThenElse { cond = $2; thenBranch = $4; elseBranch = $6 },
         mrg (snd $2) (snd $6)) }
;

lambda_expr:
  | LAMBDA lambda_args_nonempty ARROW expr
      { wrap_lambda_args (List.rev $2) $4 }
;

lambda_args_nonempty:
  | pattern_arg { [$1] }
  | lambda_args_nonempty pattern_arg { $2 :: $1 }
;

match_expr:
  | MATCH expr WITH opt_vbar match_branches
      { (Match ($2, List.rev $5), mrgn ($startpos, $endpos)) }
;

match_branches:
  | match_branch { [$1] }
  | match_branches VBAR match_branch { $3 :: $1 }
;

match_branch:
  | pattern WHEN expr ARROW expr
      { { pattern = $1; when_clause = Some $3; result = $5 } }
  | pattern ARROW expr
      { { pattern = $1; when_clause = None; result = $3 } }
;

binop:
  | app_expr { $1 }
  | binop OP_SEMI binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_COMMA binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_BAR binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_AMP binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_CMP binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop LESS binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop GREATER binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop EQUAL binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_CONS binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_COLON binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_ADD_SUB binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
  | binop OP_MUL_DIV binop
      { let op_e = (Value $2, mrgn ($startpos, $endpos)) in
        let inner = (Application (op_e, $1), mrgn ($startpos, $endpos)) in
        (Application (inner, $3), mrgn ($startpos, $endpos)) }
;

app_expr:
  | app_expr field_access %prec prec_application
      { (Application ($1, $2), mrg (snd $1) (snd $2)) }
  | field_access { $1 }
;

field_access:
  | atom { $1 }
  | field_access DOT SMALL_ID
      { (FieldAccess ($1, $3), mrg (snd $1) (mrgn ($startpos, $endpos))) }
;

atom:
  | INT     { (Const (IntLiteral $1), mrgn ($startpos, $endpos)) }
  | FLOAT   { (Const (FloatLiteral $1), mrgn ($startpos, $endpos)) }
  | STRING  { (Const (StringLiteral $1), mrgn ($startpos, $endpos)) }
  | BOOL    { (Const (BoolLiteral $1), mrgn ($startpos, $endpos)) }
  | CHAR    { (Const (CharLiteral $1), mrgn ($startpos, $endpos)) }
  | SMALL_ID { (Value $1, mrgn ($startpos, $endpos)) }
  | BIG_ID  { (Ctor $1, mrgn ($startpos, $endpos)) }
  | WILDCARD { (Value "_", mrgn ($startpos, $endpos)) }
  | LPAR operator_token RPAR { (Value $2, mrgn ($startpos, $endpos)) }
  | tuple { $1 }
  | list_cons { $1 }
  | record_init { $1 }
  | lambda_expr { $1 }
;

tuple:
  | LPAR RPAR
      { (Const UnitLiteral, mrgn ($startpos, $endpos)) }
  | LPAR expr RPAR
      { $2 }
  | LPAR expr_tuple_body RPAR
      { (TupleInit (List.rev $2), mrgn ($startpos, $endpos)) }
;

expr_tuple_body:
  | expr COMMA expr { [$3; $1] }
  | expr_tuple_body COMMA expr { $3 :: $1 }
;

list_cons:
  | LBR expr_list_semi RBR
      { let cons (a, a_r) (b, b_r) =
          let op = (Value "::", mrg a_r b_r) in
          let inner = (Application (op, (b, b_r)), b_r) in
          (Application (inner, (a, a_r)), mrg a_r b_r)
        in
        List.fold_left cons (EmptyList, mrgn ($startpos, $endpos)) $2 }
;

expr_list_semi:
  | /* empty */ { [] }
  | expr { [$1] }
  | expr_list_semi SEMICOLON expr { $3 :: $1 }
;

record_init:
  | LCBR field_binding_list RCBR
      { (RecordInit (List.rev $2), mrgn ($startpos, $endpos)) }
;

field_binding_list:
  | /* empty */ { [] }
  | field_binding { [$1] }
  | field_binding_list SEMICOLON field_binding { $3 :: $1 }
;

field_binding:
  | SMALL_ID EQUAL expr { ($1, $3) }
;

operator_token:
  | OP_PREFIX { $1 }
  | OP_MUL_DIV { $1 }
  | OP_ADD_SUB { $1 }
  | OP_COLON { $1 }
  | OP_CONS { $1 }
  | OP_CMP { $1 }
  | LESS { $1 }
  | GREATER { $1 }
  | EQUAL { $1 }
  | OP_AMP { $1 }
  | OP_BAR { $1 }
  | OP_COMMA { $1 }
  | OP_SEMI { $1 }
;

pattern:
  | pattern_atom OP_CONS pattern
      { (PatListCons ($1, $3), mrg (snd $1) (snd $3)) }
  | pattern_atom { $1 }
;

pattern_atom:
  | WILDCARD { (PatWildcard, mrgn ($startpos, $endpos)) }
  | SMALL_ID { (PatVariable $1, mrgn ($startpos, $endpos)) }
  | BIG_ID pattern_atom_opt
      { let p = match $2 with Some p -> p | None -> (PatUnit, Unknown) in
        (PatCtor ($1, p), mrgn ($startpos, $endpos)) }
  | LPAR operator_token RPAR { (PatVariable $2, mrgn ($startpos, $endpos)) }
  | LPAR RPAR { (PatUnit, mrgn ($startpos, $endpos)) }
  | LPAR pattern_tuple_body RPAR
      { match List.rev $2 with [p] -> p | xs -> (PatTuple xs, mrgn ($startpos, $endpos)) }
  | LBR RBR { (PatEmptyList, mrgn ($startpos, $endpos)) }
  | INT { (PatLiteral (IntLiteral $1), mrgn ($startpos, $endpos)) }
  | FLOAT { (PatLiteral (FloatLiteral $1), mrgn ($startpos, $endpos)) }
  | STRING { (PatLiteral (StringLiteral $1), mrgn ($startpos, $endpos)) }
  | BOOL { (PatLiteral (BoolLiteral $1), mrgn ($startpos, $endpos)) }
  | CHAR { (PatLiteral (CharLiteral $1), mrgn ($startpos, $endpos)) }
;

pattern_atom_opt:
  | /* empty */ { None }
  | pattern_atom { Some $1 }
;

pattern_tuple_body:
  | pattern_atom { [$1] }
  | pattern_tuple_body COMMA pattern_atom { $3 :: $1 }
;

%%
