open Parsing.Ast.Ast
open Parsing.ParserErrors
open Parsing.PPAst
module StringMap = Map.Make (String)

module Typec = struct
  type ground_ty = TUnit | TChar | TString | TInt | TBool | TFloat

  type ty_content =
    | TBasic of ground_ty
    | TFun of t * t
    | TTyp of string * t list
    | TVar of string
    | TFree of string
    | TAny
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
    | TVar v -> Fmt.str "'%s" v
    | TFree v -> Fmt.str "\\%s" v
    | TAny -> "any"
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
  type t_origin =
    | TorBuiltin of Typec.t
    | TorGlobal of Typec.t
    | TorArg of Typec.t
    | TorAlias of Typec.t

  type env = t_origin StringMap.t
  type typec_err = ParserErrors.t
  type typec_result = (env, typec_err) result

  val infer : program -> typec_result
  val env_to_list : env -> (string * Typec.t) list
end = struct
  open Typec

  type t_origin =
    | TorBuiltin of t
    | TorGlobal of t
    | TorArg of t
    | TorAlias of t

  let get_ty = function
    | TorBuiltin x -> x
    | TorGlobal x -> x
    | TorArg x -> x
    | TorAlias x -> x


  type env = t_origin StringMap.t
  type typec_err = ParserErrors.t
  type typec_result = (env, typec_err) result

  module StringSet = Set.Make (String)

  let env_to_list (e : env) =
    let non_builtins (k, v) =
      match v with
      | TorBuiltin _ -> None
      | TorArg x -> Some (k, x)
      | TorGlobal x -> Some (k, x)
      | TorAlias x -> Some (k, x)
    in
    StringMap.to_list e |> List.filter_map non_builtins


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
    | TypVar v -> Result.ok (TFree v, snd typ)
    | TypCtor (c, args) ->
        let* converted = mapM ty_of_typ args in
        Result.ok (TTyp (c, converted), snd typ)


  let guard pos cond msg = if cond then Result.ok () else err pos msg

  let rec apply_subst (subst : (string, t) Hashtbl.t) t =
    match fst t with
    | TBasic _ -> t
    | TFun (l, r) -> (TFun (apply_subst subst l, apply_subst subst r), snd t)
    | TTyp (n, a) -> (TTyp (n, List.map (apply_subst subst) a), snd t)
    | TFree v when Hashtbl.mem subst v -> Hashtbl.find subst v
    | TFree _ -> t
    | TVar _ -> t
    | TAny -> t
    | TTuple a -> (TTuple (List.map (apply_subst subst) a), snd t)


  let rec expand_alias_body (typ : t) =
    match fst typ with
    | TTyp (name, args) when Hashtbl.mem aliases name ->
        let ty, generics = Hashtbl.find aliases name in
        let msg = "Generic argument mismatch in type alias" in
        let* () =
          guard (snd typ) (List.length args = List.length generics) msg
        in
        let zipped = List.combine generics args in
        let subst = Hashtbl.create 16 in
        List.iter (fun (k, v) -> Hashtbl.replace subst k v) zipped;
        expand_alias_body @@ apply_subst subst ty
    | TBasic _ -> Result.ok typ
    | TFun (l, r) ->
        let* l = expand_alias_body l in
        let* r = expand_alias_body r in
        Result.ok @@ (TFun (l, r), snd typ)
    | TTyp (name, args) ->
        let* args = mapM expand_alias_body args in
        Result.ok @@ (TTyp (name, args), snd typ)
    | TVar _ -> Result.ok typ
    | TAny -> Result.ok typ
    | TFree _ -> Result.ok typ
    | TTuple vs ->
        let* vs = mapM expand_alias_body vs in
        Result.ok @@ (TTuple vs, snd typ)


  let rec rigidify (ty : t) : t =
    match fst ty with
    | TBasic t -> ty
    | TFun (l, r) -> (TFun (rigidify l, rigidify r), snd ty)
    | TTyp (name, args) -> (TTyp (name, List.map rigidify args), snd ty)
    | TVar v -> ty
    | TFree v -> (TVar v, snd ty)
    | TAny -> ty
    | TTuple args -> (TTuple (List.map rigidify args), snd ty)


  let rec most_specific' (left : t) (right : t) =
    let env = Hashtbl.create 16 in
    let rec more_speccific_inner left right =
      match (fst left, fst right) with
      | TBasic x, TBasic y ->
          let type_match = x = y in
          let msg =
            Fmt.str "Type mismatch: %s and %s" (pp_typ left) (pp_typ right)
          in
          let* () = guard (snd left) type_match msg in
          Result.ok left
      | TFun (l1, r1), TFun (l2, r2) ->
          let* t1 = more_speccific_inner l1 l2 in
          let* t2 = more_speccific_inner r1 r2 in
          Result.ok @@ (TFun (t1, t2), snd left)
      | TTyp (n1, args1), TTyp (n2, args2) ->
          let ctor_match = n1 = n2 in
          let msg =
            Fmt.str "Type ctor mismatch: %s and %s" (pp_typ left) (pp_typ right)
          in
          let* () = guard (snd left) ctor_match msg in
          let zipped = List.combine args1 args2 in
          let* args = mapM (fun (a, b) -> more_speccific_inner a b) zipped in
          Result.ok @@ (TTyp (n1, args), snd left)
      | TVar v1, TVar v2 ->
          let type_match = v1 = v2 in
          let msg =
            Fmt.str "Type var mismatch: %s and %s" (pp_typ left) (pp_typ right)
          in
          let* () = guard (snd left) type_match msg in
          Result.ok left
      | TFree v1, _ when Hashtbl.mem env v1 ->
          let t = Hashtbl.find env v1 in
          Result.ok t
      | TFree v1, _ ->
          Hashtbl.replace env v1 right;
          Result.ok right
      | _, TFree v1 when Hashtbl.mem env v1 ->
          let t = Hashtbl.find env v1 in
          Result.ok t
      | _, TFree v1 ->
          Hashtbl.replace env v1 left;
          Result.ok left
      | TTuple ts1, TTuple ts2 ->
          let zipped = List.combine ts1 ts2 in
          let* args = mapM (fun (a, b) -> more_speccific_inner a b) zipped in
          Result.ok (TTuple args, snd left)
      | _ ->
          let msg =
            Fmt.str "Failed to find most specific type of %s %s" (pp_typ left)
              (pp_typ right)
          in
          err (snd left) msg
    in
    more_speccific_inner left right


  let most_specific a b =
    Fmt.pr "Finding most specific of %s %s\n" (pp_typ a) (pp_typ b);
    let* res = most_specific' a b in
    Fmt.pr "It is %s\n" (pp_typ res);
    Result.ok res


  (* Greater is more generic, i.e. \a -> \a. Lesser can be int -> int *)
  let rec check_less (greater : t) (lesser : t) =
    let env = Hashtbl.create 16 in
    let check_with_env left right =
      Fmt.pr "Checking types: %s and %s\n" (pp_typ left) (pp_typ right);
      match (fst left, fst right) with
      | TBasic x, TBasic y -> x = y
      | TFun (l1, r1), TFun (l2, r2) -> check_less l1 l2 && check_less r1 r2
      | TTyp (n1, args1), TTyp (n2, args2) ->
          n1 = n2
          && List.length args1 = List.length args2
          && List.for_all2 check_less args1 args2
      | TVar v1, TVar v2 -> v1 = v2
      | TFree v1, _ when Hashtbl.mem env v1 ->
          let t = Hashtbl.find env v1 in
          check_less t right
      | TFree v1, _ ->
          Hashtbl.replace env v1 right;
          true
      | TAny, _ -> true
      | _, TAny -> true
      | TTuple ts1, TTuple ts2 ->
          List.length ts1 = List.length ts2 && List.for_all2 check_less ts1 ts2
      | _ -> false
    in
    check_with_env greater lesser


  let guard_less (left : t) (right : t) =
    let* left = expand_alias_body left in
    let* right = expand_alias_body right in
    let range = range_or (snd left) (snd right) in
    let msg = Fmt.str "Type mismatch: %s and %s" (pp_typ left) (pp_typ right) in
    guard range (check_less right left) msg


  let substitute (parameter : t) (arg : t) (res : t) =
    let env = Hashtbl.create 16 in
    let rec fill_subst p a =
      let* a = expand_alias_body a in
      let* p = expand_alias_body p in
      match (fst p, fst a) with
      | TBasic x, TBasic y ->
          let type_match = x = y in
          let msg = Fmt.str "Type mismatch: %s and %s" (pp_typ p) (pp_typ a) in
          guard (snd a) type_match msg
      | TFun (pl, pr), TFun (al, ar) ->
          let* () = fill_subst pl al in
          fill_subst pr ar
      | TTyp (pn, pa), TTyp (an, aa) ->
          let ctor_match = pn = an in
          let msg =
            Fmt.str "Type ctor mismatch: %s and %s" (pp_typ p) (pp_typ a)
          in
          let* () = guard (snd a) ctor_match msg in
          let zipped = List.combine pa aa in
          let* _ = mapM (fun (a, b) -> fill_subst a b) zipped in
          Result.ok ()
      | TFree v, _ when Hashtbl.mem env v ->
          let sub = Hashtbl.find env v in
          let is_same = check_less sub a in
          let msg =
            Fmt.str "Conflicting substitutions: %s and %s" (pp_typ sub)
              (pp_typ a)
          in
          guard (snd a) is_same msg
      | _, TFree v when Hashtbl.mem env v ->
          let sub = Hashtbl.find env v in
          let is_same = check_less p sub in
          let msg =
            Fmt.str "Conflicting substitutions': %s and %s" (pp_typ p)
              (pp_typ sub)
          in
          guard (snd p) is_same msg
      | TVar x, TVar y ->
          let type_match = x = y in
          let msg =
            Fmt.str "Type var mismatch: %s and %s" (pp_typ p) (pp_typ a)
          in
          guard (snd a) type_match msg
      | TFree v, _ ->
          Hashtbl.replace env v a;
          Result.ok ()
      | _, TFree v ->
          Hashtbl.replace env v p;
          Result.ok ()
      | TTuple ps, TTuple as' ->
          let zipped = List.combine ps as' in
          let* _ = mapM (fun (a, b) -> fill_subst a b) zipped in
          Result.ok ()
      | _, TAny -> Result.ok ()
      | TAny, _ -> Result.ok ()
      | _ ->
          Fmt.str "Application type error: %s and %s" (pp_typ p) (pp_typ a)
          |> err (snd a)
    in
    let* () = fill_subst parameter arg in
    Result.ok @@ apply_subst env res


  let current_id = ref 0

  let _new_tvar () =
    incr current_id;
    TFree ("a" ^ string_of_int !current_id)


  let infer_literal l =
    let ret x = Result.ok (TBasic x) in
    match l with
    | IntLiteral _ -> ret TInt
    | FloatLiteral _ -> ret TFloat
    | CharLiteral _ -> ret TChar
    | StringLiteral _ -> ret TString
    | UnitLiteral -> ret TUnit
    | BoolLiteral _ -> ret TBool


  let rec deconstruct_pattern (pat : pattern) (ty : t) (env : env) =
    let* ty = expand_alias_body ty in
    match (fst pat, fst ty) with
    | PatWildcard, _ -> Result.ok env
    | PatVariable name, _ ->
        Fmt.pr "Added value: %s = %s\n" name (pp_typ ty);
        let env = StringMap.remove name env in
        Result.ok @@ StringMap.add name (TorArg ty) env
    | PatUnit, TBasic TUnit -> Result.ok env
    | PatLiteral l, _ ->
        let* literal_ty = infer_literal l in
        let* () = guard_less ty (literal_ty, snd pat) in
        Result.ok env
    | PatTuple values, TTuple types ->
        let len_ok = List.length values = List.length types in
        let* () = guard (snd pat) len_ok "Different tuple lengths" in
        let zipped = List.combine values types in
        foldlM (fun acc (v, t) -> deconstruct_pattern v t acc) env zipped
    | PatCtor (name, pat'), _ ->
      (match StringMap.find_opt name env |> Option.map get_ty with
      | Some (TFun (arg_ty, res_ty), _) ->
          let* () = guard_less ty res_ty in
          deconstruct_pattern pat' arg_ty env
      | Some res_ty ->
          let* () = guard_less ty res_ty in
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
        let* env = deconstruct_pattern h elem_ty env in
        deconstruct_pattern t ty env
    | _ ->
        let p = PPAst.pp_pattern_ pat in
        let ty = Typec.pp_typ ty in
        let msg =
          Fmt.str "Pattern matching type mismatch for: %s and %s" p ty
        in
        err (snd pat) msg


  let rec infer_expr' (env : env) expr : (t, typec_err) result =
    match fst expr with
    | Const l ->
        let* lit = infer_literal l in
        Result.ok (lit, snd expr)
    | Value n | Ctor n ->
      (* env_to_list env |> List.iter (fun (k, v) -> Fmt.pr "%s(env) %s: %s\n" !logging_offset k (pp_typ v)); *)
      (match StringMap.find_opt n env |> Option.map get_ty with
      | Some t -> Result.ok t
      | None -> err (snd expr) @@ "Name not found: " ^ n)
    | TupleInit vs ->
        let* typs = mapM (infer_expr env) vs in
        Result.ok (TTuple typs, snd expr)
    | Lambda lam ->
        let* arg_ty = ty_of_typ (snd lam.arg) in
        let* env' = deconstruct_pattern (fst lam.arg) arg_ty env in
        let* body_ty = infer_expr env' lam.body in
        Result.ok (TFun (arg_ty, body_ty), snd expr)
    | Application (f, a) ->
        let* f_ty = infer_expr env f in
        let* a_ty = infer_expr env a in
        (match fst f_ty with
        | TFun (f_l, f_r) ->
            let* r_ty = substitute f_l a_ty f_r in
            Result.ok r_ty
        | _ ->
            let trm = PPAst.pp_expr expr in
            let typ = Typec.pp_typ f_ty in
            let msg = Fmt.str "Applying non-function (%s : %s)" trm typ in
            err (snd f) msg)
    | IfThenElse body ->
        let* cond_ty = infer_expr env body.cond in
        let* () = guard_less cond_ty (TBasic TBool, snd body.cond) in
        let* t_ty = infer_expr env body.thenBranch in
        let* e_ty = infer_expr env body.elseBranch in
        most_specific t_ty e_ty
    | Match (scrutinee, branches) ->
        let* scrutinee_ty = infer_expr env scrutinee in
        let infer_branch branch =
          let* env' = deconstruct_pattern branch.pattern scrutinee_ty env in
          let* when_clause_ty =
            Option.map (infer_expr env') branch.when_clause
            |> Option.value ~default:(Result.ok (TBasic TBool, Unknown))
          in
          let* _ = guard_less when_clause_ty (TBasic TBool, Unknown) in
          let* branch_res_ty = infer_expr env' branch.result in
          Result.ok branch_res_ty
        in
        let* typs = mapM infer_branch branches in
        (match typs with
        | [] -> err (snd expr) "At least one branch is needed"
        | h :: t -> foldlM most_specific h t)
    | LetIn (true, (PatVariable name, p), expr, in_expr) ->
        let self_ty = (_new_tvar (), p) in
        let env = StringMap.add name (TorGlobal self_ty) env in
        let* expr_ty = infer_expr env expr in
        let* () = guard_less expr_ty self_ty in
        let* res_ty = infer_expr env in_expr in
        Result.ok res_ty
    | LetIn (true, _, _, _) -> err (snd expr) "Bad recursive let"
    | LetIn (_, pat, expr, in_expr) ->
        let* expr_ty = infer_expr env expr in
        let* env = deconstruct_pattern pat expr_ty env in
        let* res_ty = infer_expr env in_expr in
        Result.ok res_ty
    | EmptyList -> Result.ok (TTyp ("Список", [ (TAny, snd expr) ]), snd expr)
    | RecordInit _ -> failwith "TODO"
    | RecordUpdate _ -> failwith "TODO"
    | FieldAccess _ -> failwith "TODO"


  and infer_expr b e =
    Fmt.pr "%s-> Inferring %s\n" !logging_offset (PPAst.pp_expr e);
    let prev_offset = !logging_offset in
    logging_offset := !logging_offset ^ "  ";
    let* ty = infer_expr' b e in
    let* ty = expand_alias_body ty in
    logging_offset := prev_offset;
    Fmt.pr "%s<- %s\n" !logging_offset (pp_typ ty);
    Result.ok ty


  let infer_let_decl name body typ env =
    let* ty = ty_of_typ typ in
    let* body_expr = infer_expr env body in
    let* () = guard_less (rigidify ty) body_expr in
    deconstruct_pattern name (fst ty, snd body_expr) env


  let add_decl_to_env name typ (env : env) =
    let* ty = ty_of_typ typ in
    match fst name with
    | PatVariable _ -> deconstruct_pattern name ty env
    | _ -> err Unknown "Cannot deconstruct this as a part of a recursive group"


  let infer_decl (env : env) (decl : decl) =
    read_line () |> ignore;
    match decl with
    | LetDecl d -> infer_let_decl d.name d.body d.typ env
    | LetDeclRecursiveGroup decls ->
        let folder e d = add_decl_to_env d.name d.typ e in
        let* env = foldlM folder env decls in
        let folder e d = infer_let_decl d.name d.body d.typ e in
        foldlM folder env decls
    | ModuleDecl _ -> failwith "TODO: Module declaration"
    | AdtDecl (name, generics, adt) ->
        let typ =
          (TTyp (name, List.map (fun (i, p) -> (TFree i, p)) generics), Unknown)
        in
        let add_ctor_as_fun env (ctor : adt_ctor_decl) =
          match ctor.typ with
          | None ->
              StringMap.add ctor.ctor_name (TorGlobal typ) env |> Result.ok
          | Some t ->
              let* ty = ty_of_typ t in
              let ty' = (TFun (ty, typ), Unknown) in
              Result.ok @@ StringMap.add ctor.ctor_name (TorGlobal ty') env
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
    let t_list a = (TTyp ("Список", [ (TFree a, Unknown) ]), Unknown) in
    let t_list_ a = (TTyp ("Список", [ a ]), Unknown) in
    let tvar x = (TFree x, Unknown) in
    let arrow3 a b c = (TFun (a, (TFun (b, c), Unknown)), Unknown) in
    let arrow a b = (TFun (a, b), Unknown) in
    let int_binop_ty = arrow3 t_int t_int t_int in
    let compare_ty = arrow3 (tvar "а") (tvar "а") t_bool in
    StringMap.of_list
      [ ("+", int_binop_ty); ("-", int_binop_ty); ("*", int_binop_ty);
        ("/", int_binop_ty); ("%", int_binop_ty);
        ("^", arrow3 t_string t_string t_string); ("<", compare_ty);
        (">", compare_ty); ("=", compare_ty); ("<>", compare_ty);
        (">=", compare_ty); ("<=", compare_ty);
        ("::", arrow3 (tvar "а") (t_list "а") (t_list "а"));
        ("@", arrow3 (t_list "а") (t_list "а") (t_list "а"));
        ("&&", arrow3 t_bool t_bool t_bool);
        ("||", arrow3 t_bool t_bool t_bool); ("не", arrow t_bool t_bool);
        ("напечатай", arrow t_string t_skib); ("считай", arrow t_skib t_string);
        ("дебаг", arrow (tvar "_") t_skib);
        ("список_из_строки", arrow t_string (t_list_ t_char));
        ("строка_из_списка", arrow (t_list_ t_char) t_string);
        ("число_из_символа", arrow t_char t_int);
        ("символ_из_числа", arrow t_int t_char);
        ("число_из_строки", arrow t_string t_int);
        ("строка_из_числа", arrow t_int t_string);
        ("прочитай_файл", arrow t_string t_string) ]
    |> StringMap.map (fun x -> TorBuiltin x)


  let infer (program : program) : typec_result =
    foldlM infer_decl empty_env program
end
