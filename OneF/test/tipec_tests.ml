open OneF.ParseTree.ParseTree
open OneF.Tipec.Tipec

let infer_expr = infer_expr_x3d

let reset () =
  type_index := 1;
  subst_v := IntMap.empty

let fail_test name message = failwith (Format.sprintf "%s: %s" name message)

let canonical_type t =
  let next = ref 0 in
  let vars = ref IntMap.empty in
  let rec go t =
    match apply_subst !subst_v t with
    | TInt -> TInt
    | TFloat -> TFloat
    | TString -> TString
    | TBool -> TBool
    | TSkib -> TSkib
    | TFun (a, b) -> TFun (go a, go b)
    | TTuple ts -> TTuple (List.map go ts)
    | TVar v -> (
        match IntMap.find_opt v !vars with
        | Some canonical -> TVar canonical
        | None ->
            let canonical = !next in
            next := canonical + 1;
            vars := IntMap.add v canonical !vars;
            TVar canonical)
  in
  go t

let equal_type a b = canonical_type a = canonical_type b

let expect_type name expected actual =
  if not (equal_type expected actual) then fail_test name "не совпал выведенный тип"

let expect_raises name f =
  try
    f ();
    fail_test name "ожидали ошибку, но выражение типизировалось"
  with Failure _ -> ()

let scheme_type name env =
  match StringMap.find_opt name env with
  | Some scheme -> instantiate scheme
  | None -> fail_test name "имя не найдено в окружении"

let fn a b = TFun (a, b)
let fn2 a b c = fn a (fn b c)
let int n = Const (IntLiteral n)
let str s = Const (StringLiteral s)
let bool b = Const (BoolLiteral b)
let unit = Const UnitLiteral
let var name = Value name
let lam arg body = Lambda { arg; body }
let app f x = Application (f, x)
let app2 f x y = app (app f x) y
let if_ cond thenBranch elseBranch = IfThenElse { cond; thenBranch; elseBranch }
let decl ?(recursive = false) name body = { name; recursive; body }
let let_in d body = LetIn (d, body)

let polymorphic_id_scheme = Forall ([ 0 ], fn (TVar 0) (TVar 0))

let tests =
  [
    ( "int literal",
      fun () -> expect_type "int literal" TInt (infer_expr StringMap.empty (int 1)) );
    ( "string literal",
      fun () -> expect_type "string literal" TString (infer_expr StringMap.empty (str "x")) );
    ( "bool literal",
      fun () -> expect_type "bool literal" TBool (infer_expr StringMap.empty (bool true)) );
    ( "unit literal",
      fun () -> expect_type "unit literal" TSkib (infer_expr StringMap.empty unit) );
    ("unknown value", fun () -> expect_raises "unknown value" (fun () -> ignore (infer_expr StringMap.empty (var "x"))));
    ( "lookup instantiates scheme",
      fun () ->
        let env = StringMap.singleton "id" polymorphic_id_scheme in
        expect_type "lookup instantiates scheme" (fn (TVar 0) (TVar 0)) (infer_expr env (var "id")) );
    ( "identity lambda",
      fun () ->
        expect_type "identity lambda" (fn (TVar 0) (TVar 0))
          (infer_expr StringMap.empty (lam (PatVariable "x") (var "x"))) );
    ( "unit lambda",
      fun () -> expect_type "unit lambda" (fn TSkib TInt) (infer_expr StringMap.empty (lam PatUnit (int 1))) );
    ( "tuple lambda",
      fun () ->
        expect_type "tuple lambda" (fn (TTuple [ TVar 0; TVar 1 ]) (TVar 0))
          (infer_expr StringMap.empty (lam (PatTuple [ PatVariable "x"; PatVariable "y" ]) (var "x"))) );
    ( "lambda shadows outer binding",
      fun () ->
        let env = StringMap.singleton "x" (monomorphic TString) in
        expect_type "lambda shadows outer binding" (fn (TVar 0) (TVar 0))
          (infer_expr env (lam (PatVariable "x") (var "x"))) );
    ( "simple application",
      fun () ->
        let env = StringMap.singleton "f" (monomorphic (fn TInt TBool)) in
        expect_type "simple application" TBool (infer_expr env (app (var "f") (int 1))) );
    ( "curried application",
      fun () ->
        let env = StringMap.singleton "add" (monomorphic (fn2 TInt TInt TInt)) in
        expect_type "curried application" TInt (infer_expr env (app2 (var "add") (int 1) (int 2))) );
    ( "bad application argument",
      fun () ->
        let env = StringMap.singleton "f" (monomorphic (fn TInt TBool)) in
        expect_raises "bad application argument" (fun () -> ignore (infer_expr env (app (var "f") (bool true)))) );
    ( "calling non-function",
      fun () -> expect_raises "calling non-function" (fun () -> ignore (infer_expr StringMap.empty (app (int 1) (int 2)))) );
    ( "occurs check through self application",
      fun () ->
        expect_raises "occurs check through self application" (fun () ->
            ignore (infer_expr StringMap.empty (lam (PatVariable "x") (app (var "x") (var "x"))))) );
    ( "if expression",
      fun () -> expect_type "if expression" TInt (infer_expr StringMap.empty (if_ (bool true) (int 1) (int 2))) );
    ( "if condition must be bool",
      fun () ->
        expect_raises "if condition must be bool" (fun () -> ignore (infer_expr StringMap.empty (if_ (int 1) (int 2) (int 3)))) );
    ( "if branches must match",
      fun () ->
        expect_raises "if branches must match" (fun () -> ignore (infer_expr StringMap.empty (if_ (bool true) (int 1) (str "x")))) );
    ( "if with polymorphic branch helper",
      fun () ->
        let env = StringMap.singleton "id" polymorphic_id_scheme in
        expect_type "if with polymorphic branch helper" TInt (infer_expr env (if_ (bool true) (app (var "id") (int 1)) (int 2))) );
    ( "simple let-in",
      fun () ->
        expect_type "simple let-in" TInt
          (infer_expr StringMap.empty (let_in (decl (PatVariable "x") (int 1)) (var "x"))) );
    ( "let shadows outer binding",
      fun () ->
        let env = StringMap.singleton "x" (monomorphic TString) in
        expect_type "let shadows outer binding" TInt (infer_expr env (let_in (decl (PatVariable "x") (int 1)) (var "x"))) );
    ( "let polymorphism",
      fun () ->
        let id_decl = decl (PatVariable "id") (lam (PatVariable "x") (var "x")) in
        let a_decl = decl (PatVariable "a") (app (var "id") (int 1)) in
        let expr = let_in id_decl (let_in a_decl (app (var "id") (bool true))) in
        expect_type "let polymorphism" TBool (infer_expr StringMap.empty expr) );
    ( "lambda argument remains monomorphic",
      fun () ->
        let expr =
          lam (PatVariable "f")
            (let_in (decl (PatVariable "x") (app (var "f") (int 1))) (app (var "f") (bool true)))
        in
        expect_raises "lambda argument remains monomorphic" (fun () -> ignore (infer_expr StringMap.empty expr)) );
    ( "top-level define",
      fun () ->
        let env = infer_program StringMap.empty [ decl (PatVariable "id") (lam (PatVariable "x") (var "x")) ] in
        expect_type "top-level define" (fn (TVar 0) (TVar 0)) (scheme_type "id" env) );
    ( "several top-level defines",
      fun () ->
        let program =
          [
            decl (PatVariable "id") (lam (PatVariable "x") (var "x"));
            decl (PatVariable "a") (app (var "id") (int 1));
            decl (PatVariable "b") (app (var "id") (bool true));
          ]
        in
        let env = infer_program StringMap.empty program in
        expect_type "top-level a" TInt (scheme_type "a" env);
        expect_type "top-level b" TBool (scheme_type "b" env) );
    ( "recursive function constrained by if",
      fun () ->
        let body = lam (PatVariable "x") (if_ (var "x") (int 1) (app (var "f") (bool false))) in
        let env = infer_program StringMap.empty [ decl ~recursive:true (PatVariable "f") body ] in
        expect_type "recursive function constrained by if" (fn TBool TInt) (scheme_type "f" env) );
    ( "recursive tuple pattern is rejected",
      fun () ->
        expect_raises "recursive tuple pattern is rejected" (fun () ->
            ignore (infer_program StringMap.empty [ decl ~recursive:true (PatTuple [ PatVariable "x"; PatVariable "y" ]) (int 1) ])) );
    ("unify equal ints", fun () -> unify TInt TInt);
    ("unify different base types", fun () -> expect_raises "unify different base types" (fun () -> unify TInt TString));
    ( "unify variable",
      fun () ->
        unify (TVar 10) TInt;
        expect_type "unify variable" TInt (apply_subst !subst_v (TVar 10)) );
    ( "tuple length mismatch",
      fun () ->
        expect_raises "tuple length mismatch" (fun () -> unify (TTuple [ TInt ]) (TTuple [ TInt; TBool ])) );
    ( "function argument mismatch",
      fun () -> expect_raises "function argument mismatch" (fun () -> unify (fn TInt TInt) (fn TBool TInt)) );
  ]

let () =
  List.iter
    (fun (name, test) ->
      reset ();
      try test () with Failure message -> failwith (Format.sprintf "test failed: %s\n%s" name message))
    tests
