open OneF.Lexer
open OneF.Lexemes

let () =
  let input = In_channel.input_all stdin in
  let tokens = Lexer.lex_string input in
  match tokens with
  | Ok tokens -> Lexemes.dump tokens |> print_endline
  | Error msg -> print_endline msg
