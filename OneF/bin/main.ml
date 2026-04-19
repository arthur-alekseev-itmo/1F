open OneF.Lexer

let input_file = ref "вход.1ф"
let output_file = ref None

let speclist = [
  ("--input", Arg.Set_string input_file, "Input file with .1ф extension");
  ("--output", Arg.String (fun x -> output_file := Some(x)), "Output file to be dumped to")
]

let read_file path =
  try 
    In_channel.with_open_text path In_channel.input_all |> Result.ok
  with
  | Sys_error e -> Format.sprintf "Error while reading file: %s" e |> Result.error

let usage_msg = "1ф --input <path> [--output <path>]"

let main () =
   let (let*) = Result.bind in
  Arg.parse speclist ignore usage_msg;
  let* lexemes = Lexer.lex_file !input_file in
  let output_path =
    match !output_file with
    | None -> !input_file ^ ".out" 
    | Some x -> x 
  in
  Lexer.dump lexemes output_path 


let () =
  match main () with 
  | Result.Ok () -> ()
  | Result.Error message -> Format.eprintf "%s" message
