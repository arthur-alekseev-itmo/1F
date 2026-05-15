open OneF.Interpreter
open OneF.Parser
open OneF.ParserErrors

let input_file = ref "вход.1ф"
let output_file = ref None
let use_stdout = ref false
let use_stdin = ref false

let speclist =
  [
    ("--input", Arg.Set_string input_file, "Input file with .1ф extension");
    ( "--output",
      Arg.String (fun x -> output_file := Some x),
      "Output file to be dumped to" );
    ( "--stdout",
      Arg.Bool (fun x -> use_stdout := x),
      "Use stdout instead of outputting to a file" );
    ( "--stdin",
      Arg.Bool (fun x -> use_stdin := x),
      "Use stdin instead of reading a file" );
  ]

let usage_msg = "1ф --input <path> [--output <path>]"

let read_file path =
  try In_channel.with_open_text path In_channel.input_all |> Result.ok
  with Sys_error e ->
    Format.sprintf "Error while reading file: %s" e |> Result.error

let read_string () =
  match !use_stdin with
  | true -> Ok (In_channel.input_all In_channel.stdin)
  | false -> read_file !input_file

let main () =
  let ( let* ) = Result.bind in
  Arg.parse speclist ignore usage_msg;
  let* input = read_string () in
  let* parse_result = Parser.program_of_string input in
  match parse_result with
  | Parser.Parsed (r, _) -> Interpreter.interpret r |> ignore |> Result.ok
  | Parser.Failed (msg, range) -> Result.ok @@ ParserErrors.print input msg range
  | Parser.HardFailed (msg, range) -> Result.ok @@ ParserErrors.print input msg range

let () =
  match main () with
  | Result.Ok () -> ()
  | Result.Error message -> Format.eprintf "%s" message
