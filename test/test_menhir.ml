let () =
  let input = In_channel.input_all stdin in
  let lexed = OneF.Parsing.Lexer.Lexer.lex_string input in
  let parsed = OneF.Parsing.MenhirDriver.parse_expr input (Result.get_ok lexed) in
  match parsed with
  | Error e ->print_endline(e)
  | _       -> print_endline("Hurra!!")