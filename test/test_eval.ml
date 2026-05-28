open OneF.Interpreter.Interpreter
open OneF.Parsing.Parser

let () =
  let input = In_channel.input_all stdin in
  let parsed = Parser.program_of_string input |> Result.get_ok in
  interpret (failwith "TODO") |> ignore
