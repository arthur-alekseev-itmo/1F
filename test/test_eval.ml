let () =
  let input = In_channel.input_all stdin in
  let _ = OneF.Parsing.Parser.Parser.program_of_string input in
  print_endline "test_eval is WIP"
