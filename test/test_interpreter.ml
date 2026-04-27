open OneF.Interpreter

let () =
  let input = In_channel.input_all stdin in
  Interpreter.eval_string input
