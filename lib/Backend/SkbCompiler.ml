open SkbClosure.SkbClosure
open SkibidIR.SkibidIR
open Parsing.Ast.Ast

module SkbCompiler = struct
  module StringMap = Map.Make (String)

  type global_sym = GlobalFunc of int | GlobalValue of int

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
        push GetCtorTag;
        let ctor_id = get_ctor_tag ctor in
        push @@ LoadConst (ScInt ctor_id);
        push @@ CmpOperator CEq;
        on_fail ();
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
      | Some (GlobalFunc x) -> push @@ FunctionAddress (Exact x)
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
    | IfThenElse _ -> failwith "TODO: ite"
    | Application (f, arg) ->
        let locals = compile_expr locals f in
        let locals = compile_expr locals arg in
        push Apply;
        locals
    | Ctor c ->
        push_value locals c;
        locals
    | RecordInit _ -> failwith "TODO"
    | RecordUpdate _ -> failwith "TODO"
    | FieldAccess _ -> failwith "TODO"
    | Match (scr, branches) ->
        let locals = compile_expr locals scr in
        let end_label = fresh_label () in
        List.fold_left (compile_branch end_label) locals branches
    | EmptyList -> failwith "TODO: elist"


  and compile_branch end_label locals (branch : match_pattern_branch) =
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
    locals


  let compile_let (decl : cc_decl_data) =
    match (fst decl.pat, decl.args) with
    | PatVariable name, _ :: _ ->
        let locals = StringMap.empty in
        let curr = curr_index () in
        Hashtbl.add function_locations name (GlobalFunc curr);
        let params = decl.args |> List.map fst in
        let locals = List.fold_left (compile_unpattern sigbus) locals params in
        compile_expr locals decl.expr |> ignore
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
    let add i x = Hashtbl.replace global_locations x (GlobalFunc (-i)) in
    let translation_fns =
      [ "напечатай"; "считай"; "дебаг"; "список_из_строки"; "строка_из_списка";
        "число_из_символа"; "символ_из_числа"; "число_из_строки";
        "строка_из_числа"; "прочитай_файл"; "не" ]
    in
    let ops = [ "+"; "-"; "*"; "/"; "%"; "^"; "@"; "&&"; "||" ] in
    let cmps = [ "<"; ">"; "="; "<>"; ">="; "<="; "::" ] in
    List.iteri add (cmps @ ops @ translation_fns);
    let builtin_cons = [ ("Конк$", -1); ("Нил$", -2) ] in
    Hashtbl.add_seq ctor_tags @@ List.to_seq builtin_cons


  let compile (decls : cc_decl list) : skb_program =
    add_globals ();
    List.iter compile_decl decls;
    skb_instructions |> Dynarray.to_array
end
