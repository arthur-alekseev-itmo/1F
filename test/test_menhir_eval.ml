let () =
  let input = In_channel.input_all stdin in
  match OneF.Parsing.Lexer.Lexer.lex_string input with
  | Error e -> print_endline e
  | Ok tokens ->
    match OneF.Parsing.MenhirDriver.parse_expr input tokens with
    | Ok expr ->
      OneF.Interpret.Interpreter.Interpreter.eval_expr expr OneF.Interpret.Interpreter.Interpreter.initial_stack
      |> OneF.Interpret.Runtime.Runtime.value_to_string
      |> print_endline
    | Error _ ->
      match OneF.Parsing.MenhirDriver.parse_program input tokens with
      | Ok prog -> OneF.Interpret.Interpreter.Interpreter.interpret prog |> ignore
      | Error e -> print_endline e
