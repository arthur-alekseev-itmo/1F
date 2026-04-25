open OneF.Lexer
open OneF.Lexemes
open OneF.Ast.Ast
open OneF.Interpreter

let input_file = ref "вход.1ф"
let output_file = ref None

let speclist =
  [
    ("--input", Arg.Set_string input_file, "Input file with .1ф extension");
    ( "--output",
      Arg.String (fun x -> output_file := Some x),
      "Output file to be dumped to" );
  ]

let usage_msg = "1ф --input <path> [--output <path>]"

let () =
  let program =
    [
      {
        name = PatUnit;
        body = Application (Value "print", Const (StringLiteral "Hi!"));
        recursive = false;
      };
    ]
  in
  Interpreter.interpret program |> ignore

let main () =
  let ( let* ) = Result.bind in
  Arg.parse speclist ignore usage_msg;
  let* lexemes = Lexer.lex_file !input_file in
  let output_path =
    match !output_file with None -> !input_file ^ ".out" | Some x -> x
  in
  Lexemes.dump output_path lexemes

let () =
  match main () with
  | Result.Ok () -> ()
  | Result.Error message -> Format.eprintf "%s" message
