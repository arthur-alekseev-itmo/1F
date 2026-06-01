open Semantics.LocalTypec
open Runtime.Runtime
open Parsing.Parser
open Result
open Parsing.ParserErrors.ParserErrors
open LNoise
open Parsing.Ast.Ast

module REPL = struct
  let ( let* ) = Result.bind

  type state =
    { types : LocalTypec.env; frame : stackframe; prog_text : string }

  type decl_or_expr = Decl of program | Expr of expr

  let parse_decl_or_expr input =
    match Parser.epxr_of_string input with
    | Result.Ok e -> ok @@ Expr e
    | Result.Error _ ->
        let* prog = Parser.program_of_string input in
        ok @@ Decl prog


  let repl_iteration command state =
    let interpret_decl = Interpreter.Interpreter.interpret_decl in
    let eval_expr = Interpreter.Interpreter.eval_expr in
    let* parsed = parse_decl_or_expr command in
    match parsed with
    | Decl decl ->
        let* types = LocalTypec.infer_program state.types decl in
        let frame = state.frame in
        let frame = List.fold_left interpret_decl frame decl in
        Fmt.pr "decls registered:\n%!";
        let new_decls =
          types
          |> StringMap.filter (fun k _ -> StringMap.mem k state.types |> not)
          |> LocalTypec.env_to_list
        in
        List.iter
          (fun (k, v) -> Fmt.pr "- %s: %s\n%!" k (Typec.pp_typ v))
          new_decls;
        ok { state with frame; types }
    | Expr expr ->
        let* typ = LocalTypec.infer_expr state.types expr in
        let frame = state.frame in
        let value = eval_expr expr frame in
        Fmt.pr "- %s : %s\n%!" (Typec.pp_typ typ) (value_to_string value);
        ok { state with frame }


  let rec loop state =
    let command = linenoise "REPL>" in
    let command = Option.value ~default:"" command in
    history_add command |> ignore;
    match repl_iteration command state with
    | Result.Ok state -> loop state
    | Result.Error e ->
        fmt "REPL" command e.message e.position |> Fmt.pr "%s\n%!";
        loop state


  let start_loop (text : string) (typec : LocalTypec.env) (frame : stackframe) =
    loop { types = typec; frame; prog_text = text }
end
