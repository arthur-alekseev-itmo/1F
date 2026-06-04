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
/* ===== ТОКЕНЫ ===== */
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token <Uchar.t> CHAR
%token <string> SMALL_ID
%token <string> BIG_ID
/* Токены операторов по группам приоритетов */
%token <string> OP_PREFIX    /* ! ~ # */
%token <string> OP_MUL_DIV   /* * / % */
%token <string> OP_ADD_SUB   /* + - */
%token <string> OP_COLON     /* : */
%token <string> OP_CONS      /* ^ @ :: */
%token <string> OP_CMP       /* <= >= <> >>= >> << и т.д. */
%token <string> LESS         /* < */
%token <string> GREATER      /* > */
%token <string> EQUAL        /* = */
%token <string> OP_AMP       /* & */
%token <string> OP_BAR       /* | */
%token <string> OP_COMMA     /* , */
%token <string> OP_SEMI      /* ; */
/* Ключевые слова */
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

/* ===== ПРИОРИТЕТЫ ===== */
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
  | ds = list(decl) EOF { ds }
;

decl:
  | d = let_decl    { d }
  | d = module_decl { d }
  | d = type_decl   { d }
;

typed_pattern:
  | LPAR p = pattern COLON t = typ RPAR { (p, t) }
;

pattern_arg:
  | tp = typed_pattern { tp }
  | p = pattern { (p, default_typ) }
;

typ:
  | t1 = typ_atom ARROW t2 = typ
      { (TypArrow (t1, t2), mrg (snd t1) (snd t2)) }
  | t = typ_atom { t }
;

typ_atom:
  | LPAR RPAR
      { (TypGround TypUnit, mrgn $loc) }
  | LPAR ts = separated_nonempty_list(COMMA, typ) RPAR
      { match ts with [t] -> t | xs -> (TypTuple xs, mrgn $loc) }
  | id = SMALL_ID
      { match ground_type_of_string id with
        | Some g -> (TypGround g, mrgn $loc)
        | None -> (TypVar id, mrgn $loc) }
  | id = BIG_ID args = option(typ_args)
      { let args = Option.value ~default:[] args in
        (TypCtor (id, args), mrgn $loc) }
;

typ_args:
  | LESS ts = separated_nonempty_list(COMMA, typ) GREATER
      { ts }
;

let_decl:
  | LET pat = pattern args = list(pattern_arg) COLON t = typ EQUAL body = expr
      { LetDecl { name = pat;
                  body = wrap_lambda_args args body;
                  typ = t } }
  | LET pat = pattern args = list(pattern_arg) EQUAL body = expr
      { LetDecl { name = pat;
                  body = wrap_lambda_args args body;
                  typ = default_typ } }
  | LET REC pat = pattern args = list(pattern_arg) COLON t = typ EQUAL body = expr
      { LetDecl { name = pat;
                  body = wrap_lambda_args args body;
                  typ = t } }
  | LET REC pat = pattern args = list(pattern_arg) EQUAL body = expr
      { LetDecl { name = pat;
                  body = wrap_lambda_args args body;
                  typ = default_typ } }
  | LET REC pat = pattern args = list(pattern_arg) COLON t = typ EQUAL body = expr
    AND pat2 = pattern args2 = list(pattern_arg) COLON t2 = typ EQUAL body2 = expr
      { LetDeclRecursiveGroup [
          { name = pat; body = wrap_lambda_args args body;
            typ = t };
          { name = pat2; body = wrap_lambda_args args2 body2;
            typ = t2 }
        ] }
  | LET REC pat = pattern args = list(pattern_arg) EQUAL body = expr
    AND pat2 = pattern args2 = list(pattern_arg) COLON t2 = typ EQUAL body2 = expr
      { LetDeclRecursiveGroup [
          { name = pat; body = wrap_lambda_args args body;
            typ = default_typ };
          { name = pat2; body = wrap_lambda_args args2 body2;
            typ = t2 }
        ] }
  | LET REC pat = pattern args = list(pattern_arg) COLON t = typ EQUAL body = expr
    AND pat2 = pattern args2 = list(pattern_arg) EQUAL body2 = expr
      { LetDeclRecursiveGroup [
          { name = pat; body = wrap_lambda_args args body;
            typ = t };
          { name = pat2; body = wrap_lambda_args args2 body2;
            typ = default_typ }
        ] }
  | LET REC pat = pattern args = list(pattern_arg) EQUAL body = expr
    AND pat2 = pattern args2 = list(pattern_arg) EQUAL body2 = expr
      { LetDeclRecursiveGroup [
          { name = pat; body = wrap_lambda_args args body;
            typ = default_typ };
          { name = pat2; body = wrap_lambda_args args2 body2;
            typ = default_typ }
        ] }
;

module_decl:
  | MODULE name = BIG_ID EQUAL STRUCT ds = list(decl) END
      { ModuleDecl { name; decls = ds } }
;

generic_vars:
  | LESS vs = separated_nonempty_list(COMMA, SMALL_ID) GREATER
      { List.map (fun v -> (v, mrgn $loc)) vs }
;

type_decl:
  | TYPE name = BIG_ID gens = option(generic_vars) EQUAL
    option(VBAR) variants = separated_nonempty_list(VBAR, adt_variant)
      { let gens = Option.value ~default:[] gens in
        AdtDecl (name, gens, variants) }
  | TYPE name = BIG_ID gens = option(generic_vars) EQUAL
    LCBR fields = separated_list(SEMICOLON, record_field) RCBR
      { let gens = Option.value ~default:[] gens in
        RecordDecl (name, gens, fields) }
  | TYPEALIAS name = BIG_ID gens = option(generic_vars) EQUAL t = typ
      { let gens = Option.value ~default:[] gens in
        AliasDecl (name, gens, t) }
;

adt_variant:
  | id = BIG_ID OF t = typ
      { { ctor_name = id; typ = Some t } }
  | id = BIG_ID
      { { ctor_name = id; typ = None } }
;

record_field:
  | name = SMALL_ID COLON t = typ
      { { field_name = name; typ = t } }
;

expr:
  | e = let_expr    { e }
  | e = if_expr     { e }
  | e = lambda_expr { e }
  | e = match_expr  { e }
  | e = binop       { e }
;

parse_expr:
  | e = expr EOF { e }
;

let_expr:
  | LET pat = pattern args = list(pattern_arg) COLON _t = typ EQUAL bound = expr IN body = expr
      { (LetIn (false, pat,
           wrap_lambda_args args bound,
           body),
         mrg (snd pat) (snd body)) }
  | LET pat = pattern args = list(pattern_arg) EQUAL bound = expr IN body = expr
      { (LetIn (false, pat,
           wrap_lambda_args args bound,
           body),
         mrg (snd pat) (snd body)) }
  | LET REC pat = pattern args = list(pattern_arg) COLON _t = typ EQUAL bound = expr IN body = expr
      { (LetIn (true, pat,
           wrap_lambda_args args bound,
           body),
         mrg (snd pat) (snd body)) }
  | LET REC pat = pattern args = list(pattern_arg) EQUAL bound = expr IN body = expr
      { (LetIn (true, pat,
           wrap_lambda_args args bound,
           body),
         mrg (snd pat) (snd body)) }
;

if_expr:
  | IF cond = expr THEN then_ = expr ELSE else_ = expr
      { (IfThenElse { cond; thenBranch = then_; elseBranch = else_ },
         mrg (snd cond) (snd else_)) }
;

lambda_expr:
  | LAMBDA pats = nonempty_list(pattern_arg) ARROW body = expr
      { wrap_lambda_args pats body }
;

match_expr:
  | MATCH e = expr WITH option(VBAR) branches = separated_nonempty_list(VBAR, match_branch)
      { (Match (e, branches), mrgn $loc) }
;

match_branch:
  | pat = pattern WHEN guard = expr ARROW result = expr
      { { pattern = pat; when_clause = Some guard; result = result } }
  | pat = pattern ARROW result = expr
      { { pattern = pat; when_clause = None; result = result } }
;

binop:
  | e = app_expr { e }
  | e1 = binop op = OP_SEMI   e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = OP_COMMA  e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = OP_BAR    e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = OP_AMP    e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = OP_CMP    e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = LESS      e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = GREATER   e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = EQUAL     e2 = binop
  | e1 = binop op = OP_CONS   e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = OP_COLON  e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = OP_ADD_SUB e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
  | e1 = binop op = OP_MUL_DIV e2 = binop
      { let op_e = (Value op, mrgn $loc) in
        let inner = (Application (op_e, e1), mrgn $loc) in
        (Application (inner, e2), mrgn $loc) }
;

app_expr:
  | e1 = app_expr e2 = field_access %prec prec_application
      { (Application (e1, e2), mrg (snd e1) (snd e2)) }
  | e = field_access { e }
;

field_access:
  | a = atom { a }
  | a = field_access DOT id = SMALL_ID
      { (FieldAccess (a, id), mrg (snd a) (mrgn $loc)) }
;

atom:
  | n = INT     { (Const (IntLiteral n), mrgn $loc) }
  | f = FLOAT   { (Const (FloatLiteral f), mrgn $loc) }
  | s = STRING  { (Const (StringLiteral s), mrgn $loc) }
  | b = BOOL    { (Const (BoolLiteral b), mrgn $loc) }
  | c = CHAR    { (Const (CharLiteral c), mrgn $loc) }
  | id = SMALL_ID { (Value id, mrgn $loc) }
  | ctor = BIG_ID  { (Ctor ctor, mrgn $loc) }
  | WILDCARD    { (Value "_", mrgn $loc) }
  | LPAR op = operator_token RPAR { (Value op, mrgn $loc) }
  | e = tuple     { e }
  | l = list_cons { l }
  | r = record_init { r }
  | e = lambda_expr { e }
;

tuple:
  | LPAR RPAR
      { (Const UnitLiteral, mrgn $loc) }
  | LPAR e = expr RPAR
      { e }
  | LPAR es = separated_nonempty_list(COMMA, expr) RPAR
      { (TupleInit es, mrgn $loc) }
;

list_cons:
  | LBR es = separated_list(SEMICOLON, expr) RBR
      { let cons (a, a_r) (b, b_r) =
          let op = (Value "::", mrg a_r b_r) in
          let inner = (Application (op, (b, b_r)), b_r) in
          (Application (inner, (a, a_r)), mrg a_r b_r)
        in
        List.fold_left cons (EmptyList, mrgn $loc) (List.rev es) }
;

record_init:
  | LCBR fields = separated_list(SEMICOLON, field_binding) RCBR
      { (RecordInit fields, mrgn $loc) }
;

field_binding:
  | name = SMALL_ID EQUAL value = expr
      { (name, value) }
;

operator_token:
  | op = OP_PREFIX { op }
  | op = OP_MUL_DIV { op }
  | op = OP_ADD_SUB { op }
  | op = OP_COLON { op }
  | op = OP_CONS { op }
  | op = OP_CMP { op }
  | op = LESS { op }
  | op = GREATER { op }
  | op = EQUAL { op }
  | op = OP_AMP { op }
  | op = OP_BAR { op }
  | op = OP_COMMA { op }
  | op = OP_SEMI { op }
;

pattern:
  | p = pattern_atom OP_CONS p2 = pattern
      { (PatListCons (p, p2), mrg (snd p) (snd p2)) }
  | p = pattern_atom { p }
;

pattern_atom:
  | WILDCARD        { (PatWildcard, mrgn $loc) }
  | id = SMALL_ID   { (PatVariable id, mrgn $loc) }
  | ctor = BIG_ID p = option(pattern_atom)
      { let p = Option.value ~default:(PatUnit, Unknown) p in
        (PatCtor (ctor, p), mrgn $loc) }
  | LPAR op = operator_token RPAR
      { (PatVariable op, mrgn $loc) }
  | LPAR RPAR       { (PatUnit, mrgn $loc) }
  | LPAR ps = separated_nonempty_list(COMMA, pattern_atom) RPAR
      { match ps with [p] -> p | xs -> (PatTuple xs, mrgn $loc) }
  | LBR RBR         { (PatEmptyList, mrgn $loc) }
  | n = INT     { (PatLiteral (IntLiteral n), mrgn $loc) }
  | f = FLOAT   { (PatLiteral (FloatLiteral f), mrgn $loc) }
  | s = STRING  { (PatLiteral (StringLiteral s), mrgn $loc) }
  | b = BOOL    { (PatLiteral (BoolLiteral b), mrgn $loc) }
  | c = CHAR    { (PatLiteral (CharLiteral c), mrgn $loc) }
;
