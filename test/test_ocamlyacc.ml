open OneF.Parsing.Ast.Ast

let rec equal_pattern (p1, _) (p2, _) =
  match p1, p2 with
  | PatUnit, PatUnit | PatWildcard, PatWildcard | PatEmptyList, PatEmptyList -> true
  | PatVariable v1, PatVariable v2 -> v1 = v2
  | PatLiteral l1, PatLiteral l2 -> l1 = l2
  | PatTuple ps1, PatTuple ps2 -> List.length ps1 = List.length ps2 && List.for_all2 equal_pattern ps1 ps2
  | PatCtor (n1, p1), PatCtor (n2, p2) -> n1 = n2 && equal_pattern p1 p2
  | PatListCons (a1, b1), PatListCons (a2, b2) -> equal_pattern a1 a2 && equal_pattern b1 b2
  | _ -> false

and equal_expr (e1, _) (e2, _) =
  match e1, e2 with
  | Const c1, Const c2 -> c1 = c2
  | Value v1, Value v2 -> v1 = v2
  | Ctor c1, Ctor c2 -> c1 = c2
  | EmptyList, EmptyList -> true
  | TupleInit es1, TupleInit es2 -> List.length es1 = List.length es2 && List.for_all2 equal_expr es1 es2
  | LetIn (rec1, p1, b1, body1), LetIn (rec2, p2, b2, body2) ->
      rec1 = rec2 && equal_pattern p1 p2 && equal_expr b1 b2 && equal_expr body1 body2
  | Lambda { arg = (a1, _); body = b1 }, Lambda { arg = (a2, _); body = b2 } ->
      equal_pattern a1 a2 && equal_expr b1 b2
  | IfThenElse { cond = c1; thenBranch = t1; elseBranch = e1 }, IfThenElse { cond = c2; thenBranch = t2; elseBranch = e2 } ->
      equal_expr c1 c2 && equal_expr t1 t2 && equal_expr e1 e2
  | Application (a1, b1), Application (a2, b2) -> equal_expr a1 a2 && equal_expr b1 b2
  | RecordInit f1, RecordInit f2 ->
      List.length f1 = List.length f2 && List.for_all2 (fun (n1, v1) (n2, v2) -> n1 = n2 && equal_expr v1 v2) f1 f2
  | RecordUpdate (e1, f1), RecordUpdate (e2, f2) ->
      equal_expr e1 e2 && List.length f1 = List.length f2 && List.for_all2 (fun (n1, v1) (n2, v2) -> n1 = n2 && equal_expr v1 v2) f1 f2
  | FieldAccess (e1, f1), FieldAccess (e2, f2) -> equal_expr e1 e2 && f1 = f2
  | Match (e1, bs1), Match (e2, bs2) -> equal_expr e1 e2 && List.length bs1 = List.length bs2 && List.for_all2 equal_branch bs1 bs2
  | _ -> false

and equal_branch b1 b2 =
  equal_pattern b1.pattern b2.pattern
  && (match b1.when_clause, b2.when_clause with
     | Some w1, Some w2 -> equal_expr w1 w2
     | None, None -> true
     | _ -> false)
  && equal_expr b1.result b2.result

and equal_typ (t1, _) (t2, _) =
  match t1, t2 with
  | TypGround g1, TypGround g2 -> g1 = g2
  | TypVar v1, TypVar v2 -> v1 = v2
  | TypArrow (a1, b1), TypArrow (a2, b2) -> equal_typ a1 a2 && equal_typ b1 b2
  | TypTuple ts1, TypTuple ts2 -> List.length ts1 = List.length ts2 && List.for_all2 equal_typ ts1 ts2
  | TypCtor (n1, a1), TypCtor (n2, a2) -> n1 = n2 && List.length a1 = List.length a2 && List.for_all2 equal_typ a1 a2
  | _ -> false

and equal_decl d1 d2 =
  match d1, d2 with
  | LetDecl { name = n1; body = b1; typ = t1 }, LetDecl { name = n2; body = b2; typ = t2 } ->
      equal_pattern n1 n2 && equal_expr b1 b2 && equal_typ t1 t2
  | LetDeclRecursiveGroup ds1, LetDeclRecursiveGroup ds2 ->
      List.length ds1 = List.length ds2 && List.for_all2 (fun d1 d2 ->
        equal_pattern d1.name d2.name && equal_expr d1.body d2.body && equal_typ d1.typ d2.typ) ds1 ds2
  | ModuleDecl { name = n1; decls = ds1 }, ModuleDecl { name = n2; decls = ds2 } ->
      n1 = n2 && List.length ds1 = List.length ds2 && List.for_all2 equal_decl ds1 ds2
  | AliasDecl (n1, vs1, t1), AliasDecl (n2, vs2, t2) ->
      n1 = n2 && List.length vs1 = List.length vs2 && equal_typ t1 t2
  | AdtDecl (n1, vs1, vs1'), AdtDecl (n2, vs2, vs2') ->
      n1 = n2 && List.length vs1 = List.length vs2
      && List.length vs1' = List.length vs2'
      && List.for_all2 (fun a b -> a.ctor_name = b.ctor_name && Option.equal equal_typ a.typ b.typ) vs1' vs2'
  | RecordDecl (n1, vs1, fs1), RecordDecl (n2, vs2, fs2) ->
      n1 = n2 && List.length vs1 = List.length vs2 && List.length fs1 = List.length fs2
      && List.for_all2 (fun a b -> a.field_name = b.field_name && equal_typ a.typ b.typ) fs1 fs2
  | _ -> false

let test_file path =
  let input = In_channel.with_open_text path In_channel.input_all in
  let lexed = OneF.Parsing.Lexer.Lexer.lex_string input in
  let tokens = Result.get_ok lexed in
  let m_prog = OneF.Parsing.MenhirDriver.parse_program input tokens in
  let o_prog = OneF.Parsing.OcamlyaccDriver.parse_program input tokens in
  match m_prog, o_prog with
  | Ok m, Ok o ->
      if List.length m <> List.length o then
        Printf.printf "  FAIL: program length %d vs %d\n%!" (List.length m) (List.length o)
      else begin
        let ok = ref true in
        for i = 0 to List.length m - 1 do
          if not (equal_decl (List.nth m i) (List.nth o i)) then begin
            Printf.printf "  FAIL: декларация %d\n%!" i;
            ok := false
          end
        done;
        if !ok then Printf.printf "  OK: program, %d decls\n%!" (List.length m)
      end
  | Error _, Ok o -> Printf.printf "  Menhir error, Ocamlyacc=%d decls\n%!" (List.length o)
  | Ok m, Error _ -> Printf.printf "  Ocamlyacc error, Menhir=%d decls\n%!" (List.length m)
  | Error _, Error _ ->
      (* try as expression *)
      let m_expr = OneF.Parsing.MenhirDriver.parse_expr input tokens in
      let o_expr = OneF.Parsing.OcamlyaccDriver.parse_expr input tokens in
      match m_expr, o_expr with
      | Ok m, Ok o ->
          if equal_expr m o then Printf.printf "  OK: expr\n%!"
          else Printf.printf "  FAIL: expr differs\n%!"
      | Error _, Ok _ -> Printf.printf "  Menhir error, Ocamlyacc OK-as-expr\n%!"
      | Ok _, Error _ -> Printf.printf "  Ocamlyacc error, Menhir OK-as-expr\n%!"
       | Error e1, Error _ -> Printf.printf "  Both error: %s\n%!" e1

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] ->
      let input = In_channel.input_all stdin in
      let lexed = OneF.Parsing.Lexer.Lexer.lex_string input in
      let tokens = Result.get_ok lexed in
      (match OneF.Parsing.MenhirDriver.parse_program input tokens,
            OneF.Parsing.OcamlyaccDriver.parse_program input tokens with
       | Ok m, Ok o ->
           let exact = ref 0 in
           for i = 0 to min (List.length m) (List.length o) - 1 do
             if List.nth m i = List.nth o i then incr exact
           done;
           let struct_ok = List.length m = List.length o
             && (try List.for_all2 equal_decl m o with _ -> false) in
           if struct_ok then
             Printf.printf "OK: %d decls, байт-в-байт: %d/%d\n%!" (List.length m) !exact (List.length m)
           else
             Printf.printf "FAIL: структурное различие\n%!"
        | Ok m, Error _ ->
            Printf.printf "  FAIL: Ocamlyacc error, Menhir=%d decls\n%!" (List.length m)
        | Error _, Ok o ->
            Printf.printf "  FAIL: Menhir error, Ocamlyacc=%d decls\n%!" (List.length o)
        | Error e, _ ->
          (match OneF.Parsing.MenhirDriver.parse_expr input tokens,
                OneF.Parsing.OcamlyaccDriver.parse_expr input tokens with
           | Ok m, Ok o ->
               if equal_expr m o then Printf.printf "  OK: expr\n%!"
               else Printf.printf "  FAIL: expr различается\n%!"
           | Error e1, Error _ -> Printf.printf "  Both error: %s\n%!" e1
           | _ -> Printf.printf "  Menhir error: %s\n%!" e))
  | files ->
      List.iter (fun f ->
        Printf.printf "=== %s ===\n%!" f;
        test_file f
      ) files
