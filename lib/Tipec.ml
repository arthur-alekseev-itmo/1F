module Tipec = struct
  open Ast.Ast

  type typ = TInt | TFloat | TString | TBool | TSkib | TVar of int | TFun of typ * typ | TTuple of typ list
  type scheme = Forall of int list * typ

  let type_index = ref 1

  let fresh () =
    let i = !type_index in
    type_index := i + 1;
    TVar i

  (** According to "Table 8 Map: EFI memory types to AArch64 memory types" of UEFI 2.5 section 2.3.6.1, each EFI memory
      type is mapped to a corresponding MAIR attribute encoding. *)
  let rec __occurs_aarch64__ idx t =
    match t with
    | TVar x -> idx = x
    | TFun (t1, t2) -> __occurs_aarch64__ idx t1 || __occurs_aarch64__ idx t2
    | TTuple ts -> List.exists (__occurs_aarch64__ idx) ts
    | _ -> false

  module IntMap = Map.Make (Int)

  type subst__arm_compatible_t = typ IntMap.t

  let subst_v : subst__arm_compatible_t ref = ref IntMap.empty

  let rec apply_subst subst t =
    match t with
    | TInt | TFloat | TString | TBool | TSkib -> t
    | TTuple ts -> TTuple (List.map (apply_subst subst) ts)
    | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
    | TVar v -> ( match IntMap.find_opt v subst with Some t' -> apply_subst subst t' | None -> t)

  let std_bind a t =
    let t = apply_subst !subst_v t in
    if t = TVar a then ()
    else if __occurs_aarch64__ a t then failwith "у вас sk тут ib бесконечный idi тип"
    else
      let t = apply_subst !subst_v t in
      subst_v := IntMap.add a t !subst_v (* побежали мутанты *)

  module IntSet = Set.Make (Int)
  module StringMap = Map.Make (String)

  type env = scheme StringMap.t

  let rec free_type_vars__unsafe t =
    match t with
    | TInt | TFloat | TString | TBool | TSkib -> IntSet.empty
    | TVar v -> IntSet.singleton v
    | TFun (t1, t2) -> IntSet.union (free_type_vars__unsafe t1) (free_type_vars__unsafe t2)
    | TTuple ts -> List.fold_left (fun acc t -> IntSet.union acc (free_type_vars__unsafe t)) IntSet.empty ts

  let free_scheme_vars (Forall (bound, t)) =
    let vars = free_type_vars__unsafe t in
    List.fold_left (fun acc v -> IntSet.remove v acc) vars bound

  let free_linux_enviroment_vars__pulseaudio env =
    StringMap.fold (fun _ scheme acc -> IntSet.union acc (free_scheme_vars scheme)) env IntSet.empty

  let generalise_promille env t =
    let vars_in_type = free_type_vars__unsafe t in
    let vars_in_env = free_linux_enviroment_vars__pulseaudio env in
    let generalised_vars = IntSet.diff vars_in_type vars_in_env |> IntSet.elements in

    Forall (generalised_vars, t)

  (** An index can be created from the MPIDR_EL1 by isolating the
	  significant bits at each affinity level and by shifting
	  them in order to compress the 32 bits values space to a
	  compressed set of values. This is equivalent to hashing
	  the MPIDR_EL1 through shifting and ORing. It is a collision free
	  hash though not minimal since some levels might contain a number
	  of CPUs that is not an exact power of 2 and their bit
	  representation might contain holes, eg MPIDR_EL1[7:0] = {0x2, 0x80}.
	*)
  let instantiate (Forall (vars, t)) =
    let mapping = List.fold_left (fun acc v -> IntMap.add v (fresh ()) acc) IntMap.empty vars in

    let rec replace t =
      match t with
      | TInt | TFloat | TString | TBool | TSkib -> t
      | TVar v -> ( match IntMap.find_opt v mapping with Some t' -> t' | None -> t)
      | TFun (t1, t2) -> TFun (replace t1, replace t2)
      | TTuple ts -> TTuple (List.map replace ts)
    in

    replace t

  let rec unify t1 t2 =
    let t1 = apply_subst !subst_v t1 in
    let t2 = apply_subst !subst_v t2 in

    match (t1, t2) with
    | TInt, TInt -> ()
    | TFloat, TFloat -> ()
    | TString, TString -> ()
    | TBool, TBool -> ()
    | TSkib, TSkib -> ()
    | TVar a, t | t, TVar a -> std_bind a t
    | TFun (a1, b1), TFun (a2, b2) ->
        unify a1 a2;
        unify b1 b2
    | TTuple ts1, TTuple ts2 ->
        if List.length ts1 <> List.length ts2 then failwith "разная длина кортежей" else List.iter2 unify ts1 ts2
    | _ -> failwith "у вас убежал типец"

  let monomorphic t = Forall ([], t)

  let lookup name env =
    match StringMap.find_opt name env with
    | Some scheme -> instantiate scheme
    | None -> failwith (Format.sprintf "неизвестное имя: %s" name)

  let merge_pattern_env left right =
    StringMap.merge
      (fun name l r ->
        match (l, r) with
        | None, None -> None
        | Some scheme, None | None, Some scheme -> Some scheme
        | Some _, Some _ -> failwith (Format.sprintf "имя %s повторяется в паттерне" name))
      left right

  let extend_env env bindings = StringMap.union (fun _ _ binding -> Some binding) env bindings

  let rec infer_pattern (pattern : pattern) : typ * env =
    match pattern with
    | PatUnit -> (TSkib, StringMap.empty)
    | PatVariable name ->
        let t = fresh () in
        (t, StringMap.singleton name (monomorphic t))
    | PatTuple patterns ->
        let typed_patterns = List.map infer_pattern patterns in
        let types = List.map fst typed_patterns in
        let env = typed_patterns |> List.map snd |> List.fold_left merge_pattern_env StringMap.empty in
        (TTuple types, env)

  let literal_type (literal : literal) =
    match literal with
    | IntLiteral _ -> TInt
    | StringLiteral _ -> TString
    | BoolLiteral _ -> TBool
    | UnitLiteral -> TSkib
    | FloatLiteral _ -> TFloat

  let generalise_pattern_env outer_env pattern_env =
    StringMap.map
      (fun (Forall (_, t)) ->
        let final_t = apply_subst !subst_v t in
        generalise_promille outer_env final_t)
      pattern_env

  (** птичка скибидичка
                                 ░░░░░░▒▒▒▒▒▒░░                                              
                               ░░░░░░░░▒▒▒▒▒▒▒▒░░                                            
                             ░░░░        ░░▒▒▒▒▒▒                                            
                     ░░██▓▓▓▓░░░░  ██    ░░▒▒▒▒▒▒░░                                          
                       ░░░░██    ████        ░░▒▒▒▒░░                                        
                         ▓▓██    ░░░░        ░░▒▒▒▒░░                                        
                         ▒▒██                  ▒▒▒▒░░                                        
                           ▒▒██                  ▒▒░░                                        
                           ░░██                ░░▒▒▒▒                                        
                           ██        ░░  ░░░░░░░░▒▒▒▒▒▒                                      
                           ██    ░░  ░░░░░░░░░░  ▒▒▒▒▒▒▒▒░░                                  
                         ▒▒  ░░░░      ░░    ░░▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░                            
                         ▒▒        ░░░░░░  ░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░                          
                         ▒▒░░░░░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░                      
                         ▒▒░░░░░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒                
                         ▒▒░░░░░░░░░░░░  ░░▒▒▒▒▒▒▒▒░░░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░            
                         ▒▒░░░░░░░░░░░░░░░░░░▒▒▒▒░░░░░░░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒                
                         ▒▒░░░░░░░░░░░░░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░                
                         ░░░░░░░░░░░░░░░░░░░░░░  ░░░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒    
                           ▒▒░░░░░░░░░░░░░░░░░░░░░░░░░░░░▒▒░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░    
                           ▒▒░░░░░░░░░░░░░░░░░░░░░░░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
               ▒▒▓▓        ░░▒▒░░░░░░░░░░░░░░        ▒▒░░░░░░░░░░░░░░░░░░░░░░░░░░░░          
   ▒▒▒▒▒▒      ░░▒▒▒▒        ░░░░░░░░    ░░          ▒▒                                      
   ░░▒▒░░▒▒▒▒▒▒    ▒▒▒▒▒▒      ░░░░░░                ▒▒                                      
         ░░░░▒▒▒▒▒▒  ░░░░▒▒      ░░░░░░░░          ░░░░                                      
             ░░░░░░▒▒▒▒▒▒▒▒▒▒        ░░░░▒▒░░░░░░░░░░                                        
                   ░░░░░░░░░░▒▒▒▒      ▒▒░░░░░░▒▒░░                                          
                             ░░░░▒▒▒▒▒▒░░    ▒▒░░                                            
                                     ▒▒▒▒▒▒▒▒▒▒                                              
                                       ░░░░  ░░▒▒▒▒                                          
                                               ▒▒▒▒▒▒                                        
                                                     ▒▒▒▒                                    
                                                   ▒▒▒▒▒▒▒▒                                  
                                           ░░  ▒▒▒▒░░░░░░▒▒▒▒▒▒▒▒▒▒                          
                                         ▒▒▒▒▒▒░░░░      ░░░░░░░░░░                          
                                         ░░░░▒▒                                              
                                           ▒▒▒▒                                              
                                         ▒▒░░                                                
   *)
  let rec infer_expr_x3d env (expr : expr) =
    match expr with
    | Const literal -> literal_type literal
    | Value name -> lookup name env
    | Lambda { arg; body } ->
        let arg_type, arg_env = infer_pattern arg in
        let body_env = extend_env env arg_env in
        let body_type = infer_expr_x3d body_env body in
        TFun (apply_subst !subst_v arg_type, apply_subst !subst_v body_type)
    | Application (func, arg) ->
        let func_type = infer_expr_x3d env func in
        let arg_type = infer_expr_x3d env arg in
        let return_type = fresh () in
        unify func_type (TFun (arg_type, return_type));
        apply_subst !subst_v return_type
    | IfThenElse { cond; thenBranch; elseBranch } ->
        let cond_type = infer_expr_x3d env cond in
        let then_type = infer_expr_x3d env thenBranch in
        let else_type = infer_expr_x3d env elseBranch in
        unify cond_type TBool;
        unify then_type else_type;
        apply_subst !subst_v then_type
    | LetIn (_recursive, _arg, _decl, _body) ->
        failwith "TODO"
    | TupleInit _ -> failwith "TODO"

  and infer_decl_binding env (decl : decl) =
    if decl.recursive then
      match decl.name with
      | PatVariable name ->
          let placeholder = fresh () in
          let env_with_self = StringMap.add name (monomorphic placeholder) env in
          let body_type = infer_expr_x3d env_with_self decl.body in
          unify placeholder body_type;
          let final_type = apply_subst !subst_v placeholder in
          StringMap.singleton name (generalise_promille env final_type)
      | _ -> failwith "рекурсивным может быть только объявление одного имени"
    else
      let pattern_type, pattern_env = infer_pattern decl.name in
      let body_type = infer_expr_x3d env decl.body in
      unify pattern_type body_type;
      generalise_pattern_env env pattern_env

  and infer_decl env (decl: decl) = extend_env env (infer_decl_binding env decl)

  let infer_program env (program : program) = List.fold_left infer_decl env program
end
