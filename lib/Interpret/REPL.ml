open Semantics.LocalTypec
open Runtime.Runtime
open Parsing.Parser
open Parsing.Lexer
open Result
open Parsing.ParserErrors.ParserErrors
open Backend.SkbClosure
open Parsing.PPAst
open LNoise

module REPL = struct
  let ( let* ) = Result.bind

  type state =
    { types : LocalTypec.env; frame : stackframe; prog_text : string }

  let repl_iteration command state =
    let interpret_decl = Interpreter.Interpreter.interpret_decl in
    let eval_expr = Interpreter.Interpreter.eval_expr in
    let* parsed = Parser.epxr_of_string command in
    (* let e = { position = Unknown; message = "Awaited single expr" } in *)
    let* typ = LocalTypec.infer_expr state.types parsed in
    (* let expr, cc_decls = SkbClosure.convert_closures_in_expr parsed in
    List.iter (fun x -> SkbClosure.pp_cc_decl x |> Fmt.pr "%s\n") cc_decls;
    PPAst.pp_expr expr |> Fmt.pr "%s\n"; *)
    let frame = state.frame in
    let value = eval_expr parsed frame in
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
