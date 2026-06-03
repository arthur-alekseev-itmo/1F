open SkbClosure.SkbClosure
open SkibidIR.SkibidIR
open Parsing.Ast.Ast

module SkbCompiler = struct
  module StringMap = Map.Make (String)

  type global_sym = GlobalFunc of int * int | GlobalValue of int

  let current_label = ref 0
  let global_value_count = ref 0

  let fresh_label () =
    current_label := !current_label + 1;
    Fmt.str "label$%d" !current_label


  let last_label () = Fmt.str "label$%d" !current_label
  let function_locations = Hashtbl.create 128
  let global_locations : (string, global_sym) Hashtbl.t = Hashtbl.create 128
  let skb_instructions = Dynarray.create ()
  let push = Dynarray.add_last skb_instructions
  let curr_index () = Dynarray.length skb_instructions
  let ctor_tags = Hashtbl.create 128

  let mark_label_here name =
    Hashtbl.add global_locations name (GlobalFunc (curr_index (), 0))


  let get_ctor_tag name =
    match Hashtbl.find_opt ctor_tags name with
    | None ->
        let length = Hashtbl.length ctor_tags in
        Hashtbl.add ctor_tags name length;
        length
    | Some x -> x


  let sigbus () =
    let curr = curr_index () in
    push @@ BranchFalse (Exact (curr + 2));
    push Sigbus


  let get_local (locals : int StringMap.t) name =
    match StringMap.find_opt name locals with
    | None ->
        let new_idx = StringMap.to_list locals |> List.length in
        (new_idx, StringMap.add name new_idx locals)
    | Some idx -> (idx, locals)


  let push_literal (lit : literal) =
    match lit with
    | IntLiteral i -> LoadConst (ScInt i)
    | FloatLiteral f -> LoadConst (ScFloat f)
    | StringLiteral s -> LoadConst (ScString s)
    | BoolLiteral b -> LoadConst (ScInt (if b then 1 else 0))
    | CharLiteral c -> LoadConst (ScInt (Uchar.to_int c))
    | UnitLiteral -> LoadConst (ScInt 0)


  let rec compile_unpattern_general save_var on_fail locals (pat : pattern) :
      int StringMap.t =
    match fst pat with
    | PatUnit ->
        push Drop;
        locals
    | PatVariable v -> save_var locals v
    | PatTuple vs ->
        let length = List.length vs in
        push @@ DeconstructTuple length;
        List.fold_left (compile_unpattern_general save_var on_fail) locals vs
    | PatCtor (ctor, inner) ->
        push Dup;
        push GetVariantTag;
        let ctor_id = get_ctor_tag ctor in
        push @@ LoadConst (ScInt ctor_id);
        push @@ CmpOperator CEq;
        on_fail ();
        push GetVariantContent;
        compile_unpattern_general save_var on_fail locals inner
    | PatWildcard ->
        push Drop;
        locals
    | PatListCons _ -> failwith "Cons must be desugared"
    | PatLiteral lit ->
        push @@ push_literal lit;
        push @@ CmpOperator CEq;
        on_fail ();
        locals
    | PatEmptyList -> failwith "Empty list must have been desugared"


  let save_local locals name =
    let index, locals = get_local locals name in
    push @@ StoreLocal index;
    locals


  let compile_unpattern on_fail = compile_unpattern_general save_local on_fail

  let save_global locals name =
    let index = !global_value_count in
    Hashtbl.add global_locations name (GlobalValue index);
    push @@ StoreGlobal index;
    global_value_count := !global_value_count + 1;
    locals


  let compile_unpattern_global on_fail =
    compile_unpattern_general save_global on_fail


  let push_value locals (name : string) =
    match StringMap.find_opt name locals with
    | Some x -> push @@ LoadLocal x
    | None ->
      (match Hashtbl.find_opt global_locations name with
      | Some (GlobalFunc (x, arity)) -> push @@ FunctionAddress (Exact x, arity)
      | Some (GlobalValue x) -> push @@ LoadGlobal (Exact x)
      | None -> push @@ LoadGlobal (Relocation name))


  let rec compile_expr locals (expr : expr) =
    match fst expr with
    | TupleInit vs ->
        let length = List.length vs in
        let locals = List.fold_left compile_expr locals vs in
        push @@ ConstructTuple length;
        locals
    | Const l ->
        push_literal l |> push;
        locals
    | Value v ->
        push_value locals v;
        locals
    | LetIn (_, pat, expr, in_expr) ->
        let _inner_locals = compile_expr locals expr in
        let locals = compile_unpattern sigbus locals pat in
        compile_expr locals in_expr
    | Lambda _ -> failwith "Lambdas must not appear in ready-to-compile AST"
    | IfThenElse ite ->
        compile_expr locals ite.cond |> ignore;
        let else_label = fresh_label () in
        let end_label = fresh_label () in
        push @@ BranchFalse (Relocation else_label);
        compile_expr locals ite.thenBranch |> ignore;
        push @@ Branch (Relocation end_label);
        mark_label_here else_label;
        compile_expr locals ite.elseBranch |> ignore;
        mark_label_here end_label;
        locals
    | Application ((Ctor c, _), expr) ->
        let ctor_tag = get_ctor_tag c in
        let locals = compile_expr locals expr in
        push @@ ConstructVariant ctor_tag;
        locals
    | Application (f, arg) ->
        let locals = compile_expr locals arg in
        let locals = compile_expr locals f in
        push Apply;
        locals
    | Ctor c ->
        Fmt.pr "TODO: Ctor appear: %s" c;
        failwith "123"
    | RecordInit _ -> failwith "TODO"
    | RecordUpdate _ -> failwith "TODO"
    | FieldAccess _ -> failwith "TODO"
    | Match (scr, branches) ->
        let locals = compile_expr locals scr in
        let end_label = fresh_label () in
        let locals =
          List.fold_left (compile_branch end_label) locals branches
        in
        mark_label_here end_label;
        locals
    | EmptyList -> failwith "TODO: elist"


  and compile_branch end_label locals (branch : match_pattern_branch) =
    push @@ Dup;
    let next_label = fresh_label () in
    let goto_next () = push @@ BranchFalse (Relocation next_label) in
    let locals = compile_unpattern goto_next locals branch.pattern in
    (* Check when_clause *)
    let locals =
      match branch.when_clause with
      | Some x -> compile_expr locals x
      | None ->
          push @@ LoadConst (ScInt 1);
          locals
    in
    goto_next ();
    let locals = compile_expr locals branch.result in
    push @@ Branch (Relocation end_label);
    mark_label_here next_label;
    locals


  let compile_let (decl : cc_decl_data) =
    match (fst decl.pat, decl.args) with
    | PatVariable name, _ :: _ ->
        let locals = StringMap.empty in
        let arity = List.length decl.args in
        let curr = curr_index () in
        Hashtbl.add global_locations name (GlobalFunc (curr, arity));
        Hashtbl.add function_locations name (GlobalFunc (curr, arity));
        let params = decl.args |> List.map fst in
        let locals =
          List.fold_left (compile_unpattern sigbus) locals (List.rev params)
        in
        compile_expr locals decl.expr |> ignore;
        push @@ Return
    | _, _ :: _ -> failwith "Cannot make a function with complex name"
    | pat, [] ->
        let locals = StringMap.empty in
        compile_expr locals decl.expr |> ignore;
        compile_unpattern_global sigbus locals (pat, Unknown) |> ignore


  let compile_decl (cc_decl : cc_decl) =
    match cc_decl with
    | CCLet l -> compile_let l
    | CCLetGroup g -> List.iter compile_let g


  let add_globals () =
    let add i x = Hashtbl.replace global_locations x (GlobalFunc (-i - 1, 2)) in
    let translation_fns =
      [ "напечатай"; "считай"; "дебаг"; "список_из_строки"; "строка_из_списка";
        "число_из_символа"; "символ_из_числа"; "число_из_строки";
        "строка_из_числа"; "прочитай_файл"; "не" ]
    in
    let ops = [ "+"; "-"; "*"; "/"; "%"; "^"; "@"; "&&"; "||" ] in
    let cmps = [ "<"; ">"; "="; "<>"; ">="; "<="; "::" ] in
    List.iteri add (cmps @ ops @ translation_fns);
    Hashtbl.iter
      (fun k (GlobalFunc (v, _)) -> Fmt.pr "%s %d\n" k v)
      global_locations;
    let builtin_cons = [ ("Конк$", -1); ("Нил$", -2) ] in
    Hashtbl.add_seq ctor_tags @@ List.to_seq builtin_cons


  let ungroup (d : cc_decl) =
    match d with CCLetGroup g -> g | CCLet c -> [ c ]


  let is_function (d : cc_decl_data) = List.is_empty d.args

  let compile (decls : cc_decl list) : skb_program =
    add_globals ();
    let decls = List.concat_map ungroup decls in
    let globs, funs = List.partition is_function decls in
    List.iter compile_let globs;
    push @@ Stop;
    List.iter compile_let funs;
    skb_instructions |> Dynarray.to_array
end
