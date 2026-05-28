open OneF.Interpreter.Interpreter
open OneF.Parsing.Parser.Parser
open OneF.Parsing.ParserErrors

let () =
  let input = In_channel.input_all stdin in
  let parse_result = program_of_string input in
  match parse_result with
  | Ok r -> interpret r |> ignore
  | Error e -> ParserErrors.print "??" input e
