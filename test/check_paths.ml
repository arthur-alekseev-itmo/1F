let () =
  let input = "3 + 2" in
  let lexed = OneF.Parsing.Lexer.Lexer.lex_string input in
  let tokens = Result.get_ok lexed in
  let parsed = OneF.Parsing.MenhirDriver.parse_expr input tokens in
  match parsed with
  | Error e -> print_endline e
  | Ok _ -> print_endline "Hurra!!"
