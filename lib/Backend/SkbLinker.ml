open SkibidIR.SkibidIR
open SkbCompiler.SkbCompiler

module SkbLinker = struct
  type locations = (string, global_sym) Hashtbl.t

  let link_ld_global locs op =
    match Hashtbl.find_opt locs op with
    | None ->
        Fmt.pr "Bad (2): %s" op;
        Fmt.failwith "Bad reloc symbol: %s" op
    | Some (GlobalFunc (i, arity)) -> FunctionAddress (Exact i, arity)
    | Some (GlobalValue i) -> LoadGlobal (Exact i)


  let link_fun_addr locs op =
    match Hashtbl.find_opt locs op with
    | None ->
        Fmt.pr "Bad (1): %s" op;
        Fmt.failwith "Bad reloc symbol: %s" op
    | Some (GlobalFunc (i, arity)) -> (Exact i, arity)
    | Some (GlobalValue _) -> Fmt.failwith "Bad fun address relocation: %s" op


  let link_instr locs op =
    match op with
    | LoadGlobal (Relocation i) -> link_ld_global locs i
    | BranchTrue (Relocation i) -> BranchTrue (fst @@ link_fun_addr locs i)
    | BranchFalse (Relocation i) -> BranchFalse (fst @@ link_fun_addr locs i)
    | Branch (Relocation i) -> Branch (fst @@ link_fun_addr locs i)
    | FunctionAddress (Relocation i, _) ->
        let i, arity = link_fun_addr locs i in
        FunctionAddress (i, arity)
    | _ -> op


  let link (locs : locations) (skibidir : skb_program) =
    Array.map (link_instr locs) skibidir
end
