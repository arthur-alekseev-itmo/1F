(* open OneF.Interpret.Interpreter
open OneF.Interpret.REPL *)
open OneF.Parsing.Parser
open OneF.Parsing.ParserErrors
open OneF.Semantics.LocalTypec
open OneF.Backend.SkbClosure
open OneF.Backend.SkbCompiler
open OneF.Backend.SkibidIR
open OneF.Backend.SkbDesugar

let input_file = ref "вход.1ф"
let output_file = ref None
let use_stdout = ref false
let use_stdin = ref false
let enable_repl = ref false

let speclist =
  [ ("--input", Arg.Set_string input_file, "Input file with .1ф extension");
    ( "--output",
      Arg.String (fun x -> output_file := Some x),
      "Output file to be dumped to" );
    ( "--stdout",
      Arg.Bool (fun x -> use_stdout := x),
      "Use stdout instead of outputting to a file" );
    ( "--stdin",
      Arg.Bool
        (fun x ->
          use_stdin := x;
          input_file := "stdin"),
      "Use stdin instead of reading a file" );
    ( "--repl",
      Arg.Bool (fun x -> enable_repl := x),
      "Enable REPL after program execution" ) ]


let usage_msg = "1ф --input <path> [--output <path>]"

let read_file path =
  try In_channel.with_open_text path In_channel.input_all |> Result.ok
  with Sys_error e ->
    Format.sprintf "Error while reading file: %s" e |> Result.error


let read_string () =
  match !use_stdin with
  | true -> Ok (read_line ())
  | false -> read_file !input_file


let main () =
  let ( let* ) = Result.bind in
  Arg.parse speclist ignore usage_msg;
  let* input = read_string () in
  match Parser.program_of_string input with
  | Result.Ok program ->
    (match LocalTypec.infer program with
    | Result.Ok _typec ->
        program
        |> SkbDesugar.desugar
        |> SkbClosure.convert_closures
        |> SkbCompiler.compile
        |> SkibidIR.pp_skibidir
        |> print_endline
        |> Result.ok
    | Result.Error e -> Result.ok @@ ParserErrors.print !input_file input e)
  | Result.Error e -> Result.ok @@ ParserErrors.print !input_file input e


let () =
  match main () with
  | Result.Ok () -> ()
  | Result.Error message -> Format.eprintf "%s" message
