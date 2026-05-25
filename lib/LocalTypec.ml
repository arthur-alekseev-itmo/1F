open Ast.Ast
open ParserErrors
module StringMap = Map.Make (String)

module Typec = struct
  type ground_ty = TUnit | TChar | TString | TInt | TBool | TFloat

  type ty_content =
    | TBasic of ground_ty
    | TFun of t * t
    | TTyp of string * t list
    | TVar of string
    | TTuple of t list

  and t = ty_content * range

  let pp_ground t =
    match t with
    | TInt -> "инт"
    | TFloat -> "дроб"
    | TString -> "строка"
    | TChar -> "символ"
    | TBool -> "бул"
    | TUnit -> "скиб"

  let rec pp_ty (force_par : bool) (typ : t) : string =
    match fst typ with
    | TBasic g -> pp_ground g
    | TVar v -> v
    | TFun (l, r) ->
        let arr = Fmt.str "%s -> %s" (pp_ty true l) (pp_ty false r) in
        if force_par then Fmt.str "(%s)" arr else arr
    | TTuple vs ->
        List.map (pp_ty false) vs |> String.concat ", " |> Fmt.str "(%s)"
    | TTyp (c, args) ->
        let args = List.map (pp_ty false) args |> String.concat ", " in
        Fmt.str "%s<%s>" c args

  let pp_typ = pp_ty false
  let equal (a : t) (b : t) = fst a = fst b
  let hash (a : t) = Hashtbl.hash (fst a)
end

module LocalTypec : sig
  type env = Typec.t StringMap.t
  type typec_err = ParserErrors.t
  type typec_result = (env, typec_err) result

  val infer : program -> typec_result
  val env_to_list : env -> (string * Typec.t) list
end = struct
  open Typec

  type env = Typec.t StringMap.t
  type typec_err = ParserErrors.t
  type typec_result = (env, typec_err) result

  module StringSet = Set.Make (String)

  type type_descr = RecordFields of StringSet.t | Ctor of string

  let env_to_list = StringMap.to_list
  let logging_offset = ref ""

  let range_or p1 p2 =
    match p1 with Unknown -> p2 | Eof when p2 <> Unknown -> p2 | _ -> p1

  let err position message =
    Result.error ({ position; message } : ParserErrors.t)

  let aliases = Hashtbl.create 128

  let foldlM f (ini : 'a) (l : 'b list) : ('a, typec_err) result =
    List.fold_left
      (fun acc x -> Result.bind acc (fun acc -> f acc x))
      (Result.ok ini) l

  let mapM (f : 'a -> ('b, typec_err) result) (l : 'a list) :
      ('b list, typec_err) result =
    foldlM (fun acc x -> Result.map (fun x -> x :: acc) (f x)) [] l
    |> Result.map List.rev

  let ( let* ) = Result.bind

  let ty_of_ground g =
    match g with
    | TypInt -> TBasic TInt
    | TypFloat -> TBasic TFloat
    | TypChar -> TBasic TChar
    | TypString -> TBasic TString
    | TypBool -> TBasic TBool
    | TypUnit -> TBasic TUnit

  let rec ty_of_typ typ =
    match fst typ with
    | TypGround g -> Result.ok (ty_of_ground g, snd typ)
    | TypArrow (left, right) ->
        let* left = ty_of_typ left in
        let* right = ty_of_typ right in
        Result.ok (TFun (left, right), snd typ)
    | TypTuple vs ->
        let* converted = mapM ty_of_typ vs in
        Result.ok (TTuple converted, snd typ)
    | TypVar v -> Result.ok (TVar v, snd typ)
    | TypCtor (c, args) ->
        let* converted = mapM ty_of_typ args in
        Result.ok (TTyp (c, converted), snd typ)

  let guard pos cond msg = if cond then Result.ok () else err pos msg

  let rec prune env (ty : t) : t =
    match fst ty with
    | TVar vname -> (
        match Hashtbl.find_opt env vname with
        | Some next_ty ->
            let actual = prune env next_ty in
            Hashtbl.replace env vname actual;
            actual
        | None -> ty)
    | _ -> ty

  let rec occurs_check env vname ty =
    match fst @@ prune env ty with
    | TVar v -> v = vname
    | TFun (a, b) -> occurs_check env vname a || occurs_check env vname b
    | TTuple tys | TTyp (_, tys) -> List.exists (occurs_check env vname) tys
    | TBasic _ -> false

  let rec expand_alias_body (typ : t) =
    match (fst typ) with
    | TTyp (name, args) when Hashtbl.mem aliases name ->
      let (ty, generics) = Hashtbl.find aliases name in
      let msg = "Generic argument mismatch in type alias" in
      let* () = guard (snd typ) (List.length args = List.length generics) msg in
      let zipped = List.combine args generics in
      let subst = Hashtbl.create 16 in
      List.iter (fun (k, v) -> Hashtbl.add subst v k) zipped;
      Result.ok @@ prune subst ty
    | _ -> Result.ok typ

  let rec unify_impl env (a : t) (b : t) =
    let* a = expand_alias_body a in
    let* b = expand_alias_body b in
    let a = prune env a in
    let b = prune env b in
    let p = range_or (snd a) (snd b) in
    match (fst a, fst b) with
    | TBasic x, TBasic y when x = y -> Result.ok a
    | TTuple lefts, TTuple rights ->
        if List.length lefts <> List.length rights then
          err p "Tuple length mismatch"
        else
          let zipped = List.combine lefts rights in
          let* checked = mapM (fun (l, r) -> unify_impl env l r) zipped in
          Result.ok (TTuple checked, p)
    | TFun (left_a, left_b), TFun (right_a, right_b) ->
        let* checked_a = unify_impl env left_a right_a in
        let* checked_b = unify_impl env left_b right_b in
        Result.ok (TFun (checked_a, checked_b), p)
    | TVar a', TVar b when a' = b -> Result.ok (TVar a', p)
    | TVar v, other ->
        let occurs = occurs_check env v b in
        let msg = Fmt.str "%s occurs in %s" v (pp_typ b) in
        let* () = guard p (not occurs) msg in
        Hashtbl.add env v (other, p);
        Result.ok (other, p)
    | other, TVar v ->
        let occurs = occurs_check env v a in
        let msg = Fmt.str "%s occurs in %s" v (pp_typ b) in
        let* () = guard p (not occurs) msg in
        Hashtbl.add env v (other, p);
        Result.ok (other, p)
    | TTyp (a_name, a_args), TTyp (b_name, b_args) ->
        let msg = Fmt.str "Cannot unify %s and %s" (pp_typ a) (pp_typ b) in
        let* () = guard p (a_name = b_name) msg in
        let generics_ok = List.length a_args = List.length b_args in
        let* () = guard p generics_ok "Type constructor arity mismatch" in
        let zipped = List.combine a_args b_args in
        let* checked = mapM (fun (l, r) -> unify_impl env l r) zipped in
        Result.ok @@ (TTyp (a_name, checked), p)
    | _ ->
        let msg = Fmt.str "Cannot unify %s with %s" (pp_typ a) (pp_typ b) in
        err p msg

  let unify subst_env (a : t) (b : t) =
    unify_impl subst_env a b

  let current_id = ref 0

  let new_tvar () =
    incr current_id;
    TVar ("'a" ^ string_of_int !current_id)

  let infer_literal l =
    let ret x = Result.ok (TBasic x) in
    match l with
    | IntLiteral _ -> ret TInt
    | FloatLiteral _ -> ret TFloat
    | CharLiteral _ -> ret TChar
    | StringLiteral _ -> ret TString
    | UnitLiteral -> ret TUnit
    | BoolLiteral _ -> ret TBool

  let rec deconstruct_pattern subst_env (pat : pattern) (ty : t) env =
    let* ty = expand_alias_body ty in
    let ty = prune subst_env ty in
    match (fst pat, fst ty) with
    | PatWildcard, _ -> Result.ok env
    | PatVariable name, _ -> Result.ok @@ StringMap.add name ty env
    | PatUnit, TBasic TUnit -> Result.ok env
    | PatLiteral l, _ ->
        let* literal_ty = infer_literal l in
        let* _ = unify subst_env (literal_ty, snd pat) ty in
        Result.ok env
    | PatTuple values, TTuple types ->
        let len_ok = List.length values = List.length types in
        let* () = guard (snd pat) len_ok "Different tuple lengths" in
        let zipped = List.combine values types in
        foldlM
          (fun acc (v, t) -> deconstruct_pattern subst_env v t acc)
          env zipped
    | PatCtor (name, pat'), _ -> (
        match StringMap.find_opt name env with
        | Some (TFun (arg_ty, res_ty), _) ->
            let* _ = unify subst_env res_ty ty in
            deconstruct_pattern subst_env pat' arg_ty env
        | Some res_ty ->
            let* _ = unify subst_env res_ty ty in
            Result.ok env
        | None -> err (snd pat) @@ Fmt.str "Cannot find constructor: %s" name)
    | PatEmptyList, TTyp (name, _) ->
        let* () =
          guard (snd pat) (name = "Список") "Empty list pattern must be list"
        in
        Result.ok env
    | PatListCons (h, t), TTyp (name, [ elem_ty ]) ->
        let* () =
          guard (snd pat) (name = "Список") "Empty list pattern must be list"
        in
        let* env = deconstruct_pattern subst_env h elem_ty env in
        deconstruct_pattern subst_env t ty env
    | _ -> failwith "Pattern matching type mismatch"

  let instantiate_poly (ty : t) : t =
    let rec inst (ty : t) : t =
      match fst ty with
      | TVar _ -> (new_tvar (), snd ty)  (* ВСЕГДА создаём свежую *)
      | TFun (l, r) -> (TFun (inst l, inst r), snd ty)
      | TTuple tys -> (TTuple (List.map inst tys), snd ty)
      | TTyp (name, args) -> (TTyp (name, List.map inst args), snd ty)
      | TBasic _ -> ty
    in
    inst ty

  let rec infer_expr' subst_env env expr : (t, typec_err) result =
    match fst expr with
    | Const l ->
        let* lit = infer_literal l in
        Result.ok (lit, snd expr)
    | Value n | Ctor n -> (
        match StringMap.find_opt n env with
        | Some t ->
            let t_instantiated = instantiate_poly t in
            Result.ok (prune subst_env t_instantiated)
        | None -> err (snd expr) @@ "Name not found: " ^ n)
    | TupleInit vs ->
        let* typs = mapM (infer_expr subst_env env) vs in
        Result.ok (TTuple typs, snd expr)
    | Lambda lam ->
        let* arg_ty = ty_of_typ (snd lam.arg) in
        let* env' = deconstruct_pattern subst_env (fst lam.arg) arg_ty env in
        let* body_ty = infer_expr subst_env env' lam.body in
        Result.ok (TFun (arg_ty, body_ty), snd expr)
    | Application (f, a) ->
        let* f_ty = infer_expr subst_env env f in
        let* a_ty = infer_expr subst_env env a in
        let res_ty = (new_tvar (), snd expr) in
        let* _ = unify subst_env (TFun (a_ty, res_ty), snd expr) f_ty in
        Result.ok (prune subst_env res_ty)
    | IfThenElse body ->
        let* cond_ty = infer_expr subst_env env body.cond in
        let* _ = unify subst_env cond_ty (TBasic TBool, snd body.cond) in
        let* t_ty = infer_expr subst_env env body.thenBranch in
        let* e_ty = infer_expr subst_env env body.elseBranch in
        unify subst_env t_ty e_ty
    | Match (scrutinee, branches) ->
        let* scrutinee_ty = infer_expr subst_env env scrutinee in
        let res_ty = (new_tvar (), snd expr) in
        let infer_branch branch =
          let* env' =
            deconstruct_pattern subst_env branch.pattern scrutinee_ty env
          in
          let* when_clause_ty =
            Option.map (infer_expr subst_env env') branch.when_clause
            |> Option.value ~default:(Result.ok (TBasic TBool, Unknown))
          in
          let* _ = unify subst_env when_clause_ty (TBasic TBool, Unknown) in
          let* branch_res_ty = infer_expr subst_env env' branch.result in
          let* _ = unify subst_env branch_res_ty res_ty in
          Result.ok res_ty
        in
        let* _ = mapM infer_branch branches in
        if branches = [] then err (snd expr) "At least one branch is needed"
        else Result.ok (prune subst_env res_ty)
    | LetIn (true, (PatVariable name, p), expr, in_expr) ->
        let self_ty = (new_tvar (), p) in
        let env = StringMap.add name self_ty env in
        let* expr_ty = infer_expr subst_env env expr in
        let* _ = unify subst_env self_ty expr_ty in
        let* res_ty = infer_expr subst_env env in_expr in
        Result.ok res_ty
    | LetIn (true, _, _, _) -> err (snd expr) "Bad recursive let"
    | LetIn (_, pat, expr, in_expr) ->
        let* expr_ty = infer_expr subst_env env expr in
        let* env = deconstruct_pattern subst_env pat expr_ty env in
        let* res_ty = infer_expr subst_env env in_expr in
        Result.ok res_ty
    | EmptyList -> Result.ok (TTyp ("Список", [ (TVar "_", Unknown) ]), snd expr)
    | RecordInit _ -> failwith "TODO"
    | RecordUpdate _ -> failwith "TODO"
    | FieldAccess _ -> failwith "TODO"

  and infer_expr a b e =
    Fmt.pr "%s-> Inferring %s\n" !logging_offset (PPAst.PPAst.pp_expr e);
    let prev_offset = !logging_offset in
    logging_offset := !logging_offset ^ "  ";
    let* ty = infer_expr' a b e in
    logging_offset := prev_offset;
    Fmt.pr "%s<- %s\n" !logging_offset (pp_typ ty);
    Result.ok ty

  let infer_let_decl subst_env name body typ env =
    let* ty = ty_of_typ typ in
    let* body_expr = infer_expr subst_env env body in
    let* t = unify subst_env body_expr ty in
    deconstruct_pattern subst_env name t env

  let add_decl_to_env subst_env name typ env =
    let* ty = ty_of_typ typ in
    match fst name with
    | PatVariable _ -> deconstruct_pattern subst_env name ty env
    | _ -> err Unknown "Cannot deconstruct this as a part of a recursive group"

  let infer_decl (env : env) (decl : decl) =
    let subst_env = Hashtbl.create 64 in
    match decl with
    | LetDecl d -> infer_let_decl subst_env d.name d.body d.typ env
    | LetDeclRecursiveGroup decls ->
        let folder e d = add_decl_to_env subst_env d.name d.typ e in
        let* env = foldlM folder env decls in
        let folder e d = infer_let_decl subst_env d.name d.body d.typ e in
        foldlM folder env decls
    | ModuleDecl _ -> failwith "TODO: Module declaration"
    | AdtDecl (name, generics, adt) ->
        let typ =
          (TTyp (name, List.map (fun (i, p) -> (TVar i, p)) generics), Unknown)
        in
        let add_ctor_as_fun env (ctor : adt_ctor_decl) =
          match ctor.typ with
          | None -> StringMap.add ctor.ctor_name typ env |> Result.ok
          | Some t ->
              let* ty = ty_of_typ t in
              let ty' = (TFun (ty, typ), Unknown) in
              Result.ok @@ StringMap.add ctor.ctor_name ty' env
        in
        foldlM add_ctor_as_fun env adt
    | AliasDecl (name, generics, ty) ->
        let* ty = ty_of_typ ty in
        Hashtbl.replace aliases name (ty, List.map fst generics);
        Result.ok env
    | RecordDecl _ -> failwith "TODO: record"

  let empty_env =
    let t_int = (TBasic TInt, Unknown) in
    let t_bool = (TBasic TBool, Unknown) in
    let t_string = (TBasic TString, Unknown) in
    let t_char = (TBasic TChar, Unknown) in
    let t_skib = (TBasic TUnit, Unknown) in
    let t_list a = (TTyp ("Список", [ (TVar a, Unknown) ]), Unknown) in
    let t_list_ a = (TTyp ("Список", [ a ]), Unknown) in
    let tvar x = (TVar x, Unknown) in
    let arrow3 a b c = (TFun (a, (TFun (b, c), Unknown)), Unknown) in
    let arrow a b = (TFun (a, b), Unknown) in
    let int_binop_ty = arrow3 t_int t_int t_int in
    let compare_ty = arrow3 (tvar "а") (tvar "а") t_bool in

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
        ("::", arrow3 (tvar "а") (t_list "а") (t_list "а"));
        ("@", arrow3 (t_list "а") (t_list "а") (t_list "а"));
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
