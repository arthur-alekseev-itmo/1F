open OneF.Parsing.ParserErrors.ParserErrors

let () =
  let input = In_channel.input_all stdin in
  match OneF.Parsing.Parser.Parser.program_of_string input with
  | Ok r -> OneF.Interpret.Interpreter.Interpreter.interpret r |> ignore
  | Error e -> print "" input e
