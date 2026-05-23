open Ast.Ast
module StringMap = Map.Make (String)

module Typec = struct
  type ground_ty = TUnit | TChar | TString | TInt | TBool | TFloat

  type ty =
    | TBasic of ground_ty
    | TFun of ty * ty
    | TTyp of string * ty list
    | TVar of string
    | TTuple of ty list

  type ty_schema = { generics : string list; body : ty }

  let pp_ground t =
    match t with
    | TInt -> "инт"
    | TFloat -> "дроб"
    | TString -> "строка"
    | TChar -> "символ"
    | TBool -> "бул"
    | TUnit -> "скиб"

  let rec pp_typ t =
    match t with
    | TBasic g -> pp_ground g
    | TTyp (name, args) ->
        let args = List.map pp_typ args |> String.concat ", " in
        Format.sprintf "%s<%s>" name args
    | TVar idx -> Format.sprintf "%s" idx
    | TFun (l, r) -> Format.sprintf "(%s -> %s)" (pp_typ l) (pp_typ r)
    | TTuple vs ->
        let vs = List.map pp_typ vs in
        String.concat " * " vs

  let pp_scheme schema =
    match schema.generics with
    | [] -> pp_typ schema.body
    | gens ->
        let q = List.map (Format.sprintf "%s") gens |> String.concat " " in
        let b = pp_typ schema.body in
        Format.sprintf "%s. %s" q b

  let monomorphic x = { generics = []; body = x }
end

module LocalTypec : sig
  type env = Typec.ty StringMap.t
  type typec_result = (env, string) result

  val infer : program -> typec_result
  val env_to_list : env -> (string * Typec.ty) list
end = struct
  open Typec

  type env = Typec.ty StringMap.t
  type typec_result = (env, string) result

  let env_to_list = StringMap.to_list

  let failf = Format.kasprintf Result.error
  let failf2 = Format.kasprintf Result.error

  let foldlM (f : 'a -> 'b -> ('a, string) result) (ini : 'a) (l : 'b list) :
      ('a, string) result =
    List.fold_left
      (fun acc x -> Result.bind acc (fun acc -> f acc x))
      (Result.ok ini) l

  let mapM (f : 'a -> ('b, string) result) (l : 'a list) :
      ('b list, string) result =
    foldlM (fun acc x -> Result.map (fun x -> x :: acc) (f x)) [] l

  let ( let* ) = Result.bind
  let print_ty t = print_endline (pp_typ t)

  let ty_of_ground g =
    match g with
    | TypInt -> TBasic TInt
    | TypFloat -> TBasic TFloat
    | TypChar -> TBasic TChar
    | TypString -> TBasic TString
    | TypBool -> TBasic TBool
    | TypUnit -> TBasic TUnit

  let rec ty_of_typ typ =
    match typ with
    | TypGround g -> Result.ok @@ ty_of_ground g
    | TypArrow (left, right) ->
        let* left = ty_of_typ left in
        let* right = ty_of_typ right in
        Result.ok @@ TFun (left, right)
    | TypTuple vs ->
        let* converted = mapM ty_of_typ vs in
        Result.ok @@ TTuple converted
    | TypVar v -> Result.ok @@ TVar v
    | _ -> failwith "TODO: Ty of typ"

  let rec unify (a : ty) (b : ty) =
    match (a, b) with
    | TBasic x, TBasic y when x = y -> Result.ok a
    | TTuple lefts, TTuple rights ->
        let zipped = List.combine lefts rights in
        let* checked = mapM (fun (a, b) -> unify a b) zipped in
        Result.ok (TTuple checked)
    | TFun (left_a, left_b), TFun (right_a, right_b) ->
        let* checked_a = unify left_a right_a in
        let* checked_b = unify left_b right_b in
        Result.ok (TFun (checked_a, checked_b))
    | TVar a, TVar b ->
        if a = b then Result.ok (TVar a)
        else
          let msg = Format.sprintf "Cannot unify %s %s" a b in
          Result.error msg
    | _ -> failf2 "TODO: Unify %s %s" (pp_typ a) (pp_typ b)

  let infer_literal l =
    let ret x = Result.ok (TBasic x) in
    match l with
    | IntLiteral _ -> ret TInt
    | FloatLiteral _ -> ret TFloat
    | CharLiteral _ -> ret TChar
    | StringLiteral _ -> ret TString
    | UnitLiteral -> ret TUnit
    | BoolLiteral _ -> ret TBool

  let rec deconstruct_pattern (pat : pattern) (ty : ty) env =
    match (pat, ty) with
    | PatVariable name, _ -> Result.ok @@ StringMap.add name ty env
    | PatTuple values, TTuple types ->
        let zipped = List.combine values types in
        foldlM (fun acc (v, t) -> deconstruct_pattern v t acc) env zipped
    | PatUnit, TBasic TUnit -> Result.ok env
    | PatLiteral l, _ ->
      let* literal_ty = infer_literal l in
      if literal_ty = ty then Result.ok env
      else let r = Format.sprintf "Bad match with literal: %s %s" (pp_typ literal_ty) (pp_typ ty) in Result.error r
    | PatWildcard, _ -> Result.ok env
    | _ -> failwith "TODO: Deconstruct pattern"

  let rec substitute vname to_what where =
    match where with
    | TBasic g -> TBasic g
    | TTyp (name, args) ->
        let args = List.map (substitute vname to_what) args in
        TTyp (name, args)
    | TVar v when v = vname -> to_what
    | TVar v -> TVar v
    | TFun (l, r) ->
        TFun (substitute vname to_what l, substitute vname to_what r)
    | TTuple vs ->
        let vs = List.map (substitute vname to_what) vs in
        TTuple vs

  let rec check_and_substitute param body arg =
    let cas = check_and_substitute in
    match (param, arg) with
    | TVar v, _ -> Result.ok @@ substitute v arg body
    | TBasic _, _ ->
      let* _ = unify param arg in
      Result.ok body
    | TTuple params, TTuple args ->
        let zipped = List.combine params args in
        foldlM (fun acc (k, v) -> cas k acc v) body zipped
    | TFun (la, lr), TFun (ra, rr) ->
        let* im = cas la body ra in
        cas lr im rr
    | _ -> failwith @@ "TODO: sub " ^ pp_typ param ^ " and " ^ pp_typ arg

  let rec infer_expr env expr =
    match expr with
    | Const l -> infer_literal l
    | Value n ->
        let found = StringMap.find_opt n env |> Option.map Result.ok in
        found |> Option.value ~default:(Result.error @@ "Name not found: " ^ n)
    | TupleInit vs ->
        let* typs = mapM (infer_expr env) vs in
        Result.ok @@ TTuple typs
    | Lambda lam ->
        let* arg_ty = ty_of_typ (snd lam.arg) in
        let* env' = deconstruct_pattern (fst lam.arg) arg_ty env in
        let* body_ty = infer_expr env' lam.body in
        Result.ok (TFun (arg_ty, body_ty))
    | Application (f, a) ->
        let* f_ty = infer_expr env f in
        let* a_ty = infer_expr env a in
        infer_app f_ty a_ty env
    | IfThenElse body ->
        let* cond_ty = infer_expr env body.cond in
        print_ty cond_ty;
        let* _ = unify cond_ty (TBasic TBool) in
        let* e_ty = infer_expr env body.elseBranch in
        let* t_ty = infer_expr env body.thenBranch in
        unify e_ty t_ty
    | Match (scrutinee, branches) ->
        let* scrutinee_ty = infer_expr env scrutinee in
        let infer_branch branch =
          let* env' = deconstruct_pattern branch.pattern scrutinee_ty env in
          let* when_clause_ty =
            Option.map (infer_expr env') branch.when_clause
            |> Option.value ~default:(Result.ok (TBasic TBool))
          in
          let* _ = unify when_clause_ty (TBasic TBool) in
          infer_expr env' branch.result
        in
        let* inferred_branches = mapM infer_branch branches in
        (match inferred_branches with | [] -> Result.error "At least one branch is needed" | h :: t -> foldlM unify h t)
    | _ -> failwith "TODO: Infer expr"

  and infer_app (f : ty) (arg : ty) _ =
    print_endline "Application";
    print_ty f;
    print_ty arg;
    match f with
    | TFun (param, body) ->
      let* res = check_and_substitute param body arg in
      print_ty res;
      Result.ok res
    | t -> failf "Cannot apply non-function: %s" (pp_typ t)

  let infer_let_decl name body typ env =
    let* ty = ty_of_typ typ in
    let* body_expr = infer_expr env body in
    let* t = unify body_expr ty in
    deconstruct_pattern name t env

  let add_decl_to_env name typ env =
    let* ty = ty_of_typ typ in
    match name with
    | PatVariable _ -> deconstruct_pattern name ty env
    | _ -> Result.error "Cannot deconstruct this as a part of a recursive group"

  let infer_decl (env : env) (decl : decl) =
    match decl with
    | LetDecl d -> infer_let_decl d.name d.body d.typ env
    | LetDeclRecursiveGroup decls ->
      let* env = foldlM (fun e d -> add_decl_to_env d.name d.typ e) env decls in
      foldlM (fun e d -> infer_let_decl d.name d.body d.typ e) env decls
    | ModuleDecl _ -> failwith "TODO: Module declaration"

  let empty_env =
    let t_int = TBasic TInt in
    let t_bool = TBasic TBool in
    let t_string = TBasic TString in
    let t_char = TBasic TChar in
    let t_skib = TBasic TUnit in
    let t_list a = TTyp ("List", [ TVar a ]) in
    let t_list_ a = TTyp ("List", [ a ]) in
    let tvar x = TVar x in
    let arrow3 a b c = TFun (a, TFun (b, c)) in
    let arrow a b = TFun (a, b) in
    let int_binop_ty = arrow3 t_int t_int t_int in
    let compare_ty = arrow3 (tvar "#а") (tvar "#а") t_bool in

    StringMap.of_list
      [
        ("+", int_binop_ty);
        ("-", int_binop_ty);
        ("*", int_binop_ty);
        ("/", int_binop_ty);
        ("%", int_binop_ty);
        ("^", arrow3 t_string t_string t_string);
        ("<", compare_ty);
        (">", compare_ty);
        ("=", compare_ty);
        ("<>", compare_ty);
        (">=", compare_ty);
        ("<=", compare_ty);
        ("::", arrow3 (tvar "#а") (t_list "#а") (t_list "#а"));
        ("@", arrow3 (t_list "#а") (t_list "#а") (t_list "#а"));
        ("&&", arrow3 t_bool t_bool t_bool);
        ("||", arrow3 t_bool t_bool t_bool);
        ("не", arrow t_bool t_bool);
        ("напечатай", arrow t_string t_skib);
        ("считай", arrow t_skib t_string);
        ("дебаг", arrow (tvar "_") t_skib);
        ("список_из_строки", arrow t_string (t_list_ t_char));
        ("строка_из_списка", arrow (t_list_ t_char) t_string);
        ("число_из_символа", arrow t_char t_int);
        ("символ_из_числа", arrow t_int t_char);
        ("число_из_строки", arrow t_string t_int);
        ("строка_из_числа", arrow t_int t_string);
        ("прочитай_файл", arrow t_string t_string);
      ]

  let infer (program : program) : typec_result =
    foldlM infer_decl empty_env program
end
