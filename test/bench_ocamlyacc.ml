open OneF.Parsing.Ast.Ast

let lex_string s = OneF.Parsing.Lexer.Lexer.lex_string s

let time_us f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  let us = (t1 -. t0) *. 1e6 in
  (r, us)

let fmt_us us =
  if us >= 1e6 then Printf.sprintf "%.2f s" (us /. 1e6)
  else if us >= 1000. then Printf.sprintf "%.2f ms" (us /. 1000.)
  else Printf.sprintf "%.0f us" us

let run_bench label iterations f =
  let (_, total) = time_us (fun () ->
    for _ = 1 to iterations do
      let (_ : unit) = f () in ()
    done
  ) in
  let avg = total /. float iterations in
  Printf.printf "  %s: среднее %s (x%d = %s всего)\n%!"
    label (fmt_us avg) iterations (fmt_us total)

let rec count_pattern (p, _r) =
  match p with
  | PatUnit | PatWildcard | PatEmptyList | PatVariable _ | PatLiteral _ -> 1
  | PatTuple ps -> 1 + List.fold_left (fun a p -> a + count_pattern p) 0 ps
  | PatCtor (_, p) -> 1 + count_pattern p
  | PatListCons (a, b) -> 1 + count_pattern a + count_pattern b

let rec count_expr (e, _r) =
  match e with
  | Const _ | Value _ | Ctor _ | EmptyList -> 1
  | TupleInit es -> 1 + List.fold_left (fun a e -> a + count_expr e) 0 es
  | LetIn (_, p, bound, body) -> 1 + count_pattern p + count_expr bound + count_expr body
  | Lambda { arg = (a, _t); body } -> 1 + count_pattern a + count_expr body
  | IfThenElse { cond; thenBranch; elseBranch } ->
      1 + count_expr cond + count_expr thenBranch + count_expr elseBranch
  | Application (a, b) -> 1 + count_expr a + count_expr b
  | RecordInit fields -> 1 + List.fold_left (fun a (_, e) -> a + count_expr e) 0 fields
  | RecordUpdate (e, fields) ->
      1 + count_expr e + List.fold_left (fun a (_, e) -> a + count_expr e) 0 fields
  | FieldAccess (e, _) -> 1 + count_expr e
  | Match (e, branches) ->
      1 + count_expr e + List.fold_left (fun a b -> a + count_branch b) 0 branches

and count_branch b =
  let w = match b.when_clause with Some e -> count_expr e | None -> 0 in
  count_pattern b.pattern + w + count_expr b.result

let rec count_decl d =
  match d with
  | LetDecl { name; body; _ } -> count_pattern name + count_expr body
  | LetDeclRecursiveGroup ds ->
      List.fold_left (fun a { name; body; _ } -> a + count_pattern name + count_expr body) 0 ds
  | ModuleDecl { decls; _ } -> List.fold_left (fun a d -> a + count_decl d) 0 decls
  | AliasDecl _ -> 1
  | AdtDecl _ -> 1
  | RecordDecl _ -> 1

let () =
  let input = In_channel.input_all stdin in
  let lines = String.split_on_char '\n' input in
  let n_lines = List.length lines in
  Printf.printf "Вход: %d строк, %d байт\n%!" n_lines (String.length input);

  let (tokens, lex_us) = time_us (fun () ->
    match lex_string input with
    | Ok t -> t
    | Error e -> failwith e
  ) in
  Printf.printf "Лексер: %s\n%!" (fmt_us lex_us);
  Printf.printf "  токенов: %d\n%!" (List.length tokens);

  (match OneF.Parsing.MenhirDriver.parse_program input tokens with
   | Ok prog ->
       let nodes = List.fold_left (fun a d -> a + count_decl d) 0 prog in
       Printf.printf "MenhirParser:       %d деклараций, %d узлов AST\n%!"
         (List.length prog) nodes
   | Error _ ->
     match OneF.Parsing.MenhirDriver.parse_expr input tokens with
     | Ok expr ->
         let nodes = count_expr expr in
         Printf.printf "MenhirParser:       1 выражение, %d узлов AST\n%!" nodes
     | Error e -> Printf.printf "MenhirParser: ошибка: %s\n%!" e);
  (match OneF.Parsing.OcamlyaccDriver.parse_program input tokens with
   | Ok prog ->
       let nodes = List.fold_left (fun a d -> a + count_decl d) 0 prog in
       Printf.printf "OcamlyaccParser:    %d деклараций, %d узлов AST\n%!"
         (List.length prog) nodes
   | Error _ ->
     match OneF.Parsing.OcamlyaccDriver.parse_expr input tokens with
     | Ok expr ->
         let nodes = count_expr expr in
         Printf.printf "OcamlyaccParser:    1 выражение, %d узлов AST\n%!" nodes
     | Error e -> Printf.printf "OcamlyaccParser: ошибка: %s\n%!" e);

  Printf.printf "\n--- Парсеры (только парсинг, лексер исключён) ---\n%!";

  let iterations =
    let len = List.length tokens in
    if len = 0 then 1
    else max 1 (min 1000 (50000 / len))
  in

  run_bench "MenhirParser" iterations (fun () ->
    match OneF.Parsing.MenhirDriver.parse_expr input tokens with
    | Ok _ -> ()
    | Error _ ->
      match OneF.Parsing.MenhirDriver.parse_program input tokens with
      | Ok _ -> ()
      | Error _ -> ());
  run_bench "OcamlyaccParser" iterations (fun () ->
    match OneF.Parsing.OcamlyaccDriver.parse_expr input tokens with
    | Ok _ -> ()
    | Error _ ->
      match OneF.Parsing.OcamlyaccDriver.parse_program input tokens with
      | Ok _ -> ()
      | Error _ -> ());

  Printf.printf "\n--- Полный цикл (лексинг + парсинг) ---\n%!";
  let small_iter = max 1 (min 100 (10000 / max 1 (String.length input))) in
  run_bench "MenhirParser" small_iter (fun () ->
    match lex_string input with
    | Ok t ->
      (match OneF.Parsing.MenhirDriver.parse_expr input t with
       | Ok _ -> ()
       | Error _ -> let _ = OneF.Parsing.MenhirDriver.parse_program input t in ())
    | Error _ -> ());
  run_bench "OcamlyaccParser" small_iter (fun () ->
    match lex_string input with
    | Ok t ->
      (match OneF.Parsing.OcamlyaccDriver.parse_expr input t with
       | Ok _ -> ()
       | Error _ -> let _ = OneF.Parsing.OcamlyaccDriver.parse_program input t in ())
    | Error _ -> ())
