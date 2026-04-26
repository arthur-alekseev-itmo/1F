module Tipec = struct
  type typ = TInt | TString | TBool | TSkib | TVar of int | TFun of typ * typ | TTuple of typ list
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
    | TInt | TString | TBool | TSkib -> t
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

  let rec free_type_vars__unsafe t =
    match t with
    | TInt | TString | TBool | TSkib -> IntSet.empty
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
      | TInt | TString | TBool | TSkib -> t
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
end
