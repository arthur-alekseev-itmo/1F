open OneF.Interpreter
open OneF.Parser
open OneF.ParserErrors

let () =
  let input = In_channel.input_all stdin in
  let parse_result = Parser.program_of_string input |> Result.get_ok in
  match parse_result with
  | Parser.Parsed (r, _) -> Interpreter.interpret r |> ignore
  | Parser.Failed (msg, range) -> ParserErrors.print input msg range
  | Parser.HardFailed (msg, range) -> ParserErrors.print input msg range
