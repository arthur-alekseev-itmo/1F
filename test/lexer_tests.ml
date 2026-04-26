open OneF.Lexer.Lexer

let fail_test name message = failwith (Format.sprintf "%s: %s" name message)
let eq = Operator "="
let unit = [ LPar; RPar ]

let rec collect gen =
  let result, start_pos, end_pos = gen () in
  match result with
  | Error message -> Error message
  | Ok Eof -> Ok []
  | Ok token ->
      Result.map (fun tail -> (token, start_pos, end_pos) :: tail) (collect gen)

let lex input = collect (token_gen_of_string input)
let tokens_only triples = List.map (fun (token, _, _) -> token) triples
let show_token = show

let show_tokens tokens =
  tokens |> List.map show_token |> String.concat "; " |> Format.sprintf "[%s]"

let expect_tokens name expected input =
  match lex input with
  | Error message ->
      fail_test name (Format.sprintf "unexpected lexer error: %s" message)
  | Ok actual_triples ->
      let actual = tokens_only actual_triples in
      if actual <> expected then
        fail_test name
          (Format.sprintf "expected %s, got %s" (show_tokens expected)
             (show_tokens actual))

let expect_error name input =
  match lex input with
  | Error _ -> ()
  | Ok actual_triples ->
      fail_test name
        (Format.sprintf "expected lexer error, got %s"
           (show_tokens (tokens_only actual_triples)))

let nth_token name index input =
  match lex input with
  | Error message ->
      fail_test name (Format.sprintf "unexpected lexer error: %s" message)
  | Ok tokens -> (
      match List.nth_opt tokens index with
      | Some token -> token
      | None ->
          fail_test name (Format.sprintf "missing token at index %d" index))

let expect_position name index ~start_line ~start_char ~end_line ~end_char input
    =
  let _, start_pos, end_pos = nth_token name index input in
  if
    start_pos.pos_lnum <> start_line
    || start_pos.pos_cnum <> start_char
    || end_pos.pos_lnum <> end_line
    || end_pos.pos_cnum <> end_char
  then
    fail_test name
      (Format.sprintf
         "expected line %d char %d to line %d char %d, got line %d char %d to \
          line %d char %d"
         start_line start_char end_line end_char start_pos.pos_lnum
         start_pos.pos_cnum end_pos.pos_lnum end_pos.pos_cnum)

let write_text path content =
  Out_channel.with_open_text path (fun channel ->
      Out_channel.output_string channel content)

let read_text path = In_channel.with_open_text path In_channel.input_all

let with_temp_file prefix suffix content f =
  let path = Filename.temp_file prefix suffix in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      write_text path content;
      f path)

let expect_file_tokens name expected input =
  with_temp_file "onef-lexer-" ".1f" input (fun path ->
      match lex_file path with
      | Error message ->
          fail_test name (Format.sprintf "unexpected lexer error: %s" message)
      | Ok actual_triples ->
          let actual = tokens_only actual_triples in
          if actual <> expected then
            fail_test name
              (Format.sprintf "expected %s, got %s" (show_tokens expected)
                 (show_tokens actual)))

let expect_file_error name input =
  with_temp_file "onef-lexer-error-" ".1f" input (fun path ->
      match lex_file path with
      | Error _ -> ()
      | Ok actual_triples ->
          fail_test name
            (Format.sprintf "expected lexer error, got %s"
               (show_tokens (tokens_only actual_triples))))

let expect_text_file name expected path =
  let actual = read_text path in
  if actual <> expected then
    fail_test name
      (Format.sprintf "expected file content:\n%s\n\ngot:\n%s" expected actual)

let tests =
  [
    ( "keywords",
      fun () ->
        expect_tokens "keywords"
          [ Let; Rec; In; If; Then; Else; Lambda ]
          "пусть рек в если то иначе лямбда" );
    ( "bool literals",
      fun () ->
        expect_tokens "bool literals"
          [ BoolLiteral true; BoolLiteral false ]
          "да нет" );
    ( "identifiers",
      fun () ->
        expect_tokens "identifiers"
          [
            SmallIdentifier "x";
            SmallIdentifier "x1";
            SmallIdentifier "_name";
            SmallIdentifier "привет";
            SmallIdentifier "имя_2";
          ]
          "x x1 _name привет имя_2" );
    ( "integer literals",
      fun () ->
        expect_tokens "integer literals"
          [ IntLiteral 0; IntLiteral 1; IntLiteral 42; IntLiteral 100500 ]
          "0 1 42 100500" );
    ( "float literals",
      fun () ->
        expect_tokens "float literals"
          [
            FloatLiteral 0.0;
            FloatLiteral 3.14;
            FloatLiteral 1.0;
            FloatLiteral 0.5;
          ]
          "0.0 3.14 1. .5" );
    ( "string literals",
      fun () ->
        expect_tokens "string literals"
          [ StringLiteral "abc"; StringLiteral ""; StringLiteral "привет" ]
          "\"abc\" \"\" \"привет\"" );
    ( "operators",
      fun () ->
        expect_tokens "operators"
          [
            Operator "+";
            Operator "-";
            Operator "*";
            Operator "/";
            Operator "==";
            Operator "<=";
            Operator ">=";
            Operator "!=";
            Operator "&&";
            Operator "||";
          ]
          "+ - * / == <= >= != && ||" );
    ("arrow token", fun () -> expect_tokens "arrow token" [ Arrow ] "->");
    ( "equals is not operator",
      fun () -> expect_tokens "equals is not operator" [ eq ] "=" );
    ( "parentheses and comma",
      fun () ->
        expect_tokens "parentheses and comma" [ LPar; RPar; Comma ] "( ) ," );
    ("unit literal", fun () -> expect_tokens "unit literal" unit "()");
    ( "whitespace is ignored",
      fun () ->
        expect_tokens "whitespace is ignored"
          [ Let; SmallIdentifier "x"; eq; IntLiteral 42 ]
          "пусть\tx\n\n=\n \t42" );
    ( "simple declaration",
      fun () ->
        expect_tokens "simple declaration"
          [ Let; SmallIdentifier "x"; eq; IntLiteral 42 ]
          "пусть x = 42" );
    ( "lambda expression",
      fun () ->
        expect_tokens "lambda expression"
          [ Lambda; SmallIdentifier "x"; Arrow; SmallIdentifier "x" ]
          "лямбда x -> x" );
    ( "if expression",
      fun () ->
        expect_tokens "if expression"
          [ If; BoolLiteral true; Then; IntLiteral 1; Else; IntLiteral 0 ]
          "если да то 1 иначе 0" );
    ( "positions single line",
      fun () ->
        let input = "пусть x" in
        expect_position "positions single line Let" 0 ~start_line:1
          ~start_char:0 ~end_line:1 ~end_char:5 input;
        expect_position "positions single line identifier" 1 ~start_line:1
          ~start_char:6 ~end_line:1 ~end_char:7 input );
    ( "positions multiline",
      fun () ->
        let input = "пусть\nx" in
        expect_position "positions multiline Let" 0 ~start_line:1 ~start_char:0
          ~end_line:1 ~end_char:5 input;
        expect_position "positions multiline identifier" 1 ~start_line:2
          ~start_char:6 ~end_line:2 ~end_char:7 input );
    ( "unknown character error",
      fun () -> expect_error "unknown character error" "#" );
    ( "unterminated string error",
      fun () -> expect_error "unterminated string error" "\"abc" );
    ( "lex file reads real file",
      fun () ->
        expect_file_tokens "lex file reads real file"
          [ Let; SmallIdentifier "x"; eq; IntLiteral 42 ]
          "пусть x = 42" );
    ( "lex file reports lexical error",
      fun () -> expect_file_error "lex file reports lexical error" "пусть #" );
    ( "lex file reports missing file",
      fun () ->
        let path = Filename.temp_file "onef-missing-" ".1f" in
        Sys.remove path;
        match lex_file path with
        | Error message ->
            if
              not
                (String.starts_with ~prefix:"Error while reading file:" message)
            then
              fail_test "lex file reports missing file"
                (Format.sprintf "unexpected error message: %s" message)
        | Ok _ ->
            fail_test "lex file reports missing file" "expected file read error"
    );
    ( "dump writes token coordinates",
      fun () ->
        with_temp_file "onef-dump-input-" ".1f" "пусть x" (fun input_path ->
            let output_path = input_path ^ ".out" in
            Fun.protect
              ~finally:(fun () ->
                if Sys.file_exists output_path then Sys.remove output_path)
              (fun () ->
                match lex_file input_path with
                | Error message ->
                    fail_test "dump writes token coordinates" message
                | Ok lexemes -> (
                    match dump_file output_path lexemes with
                    | Error message ->
                        fail_test "dump writes token coordinates" message
                    | Ok () ->
                        expect_text_file "dump writes token coordinates"
                          "[line: 1, char: 0-5] пусть\n\
                           [line: 1, char: 6-7] имя (маленькое) x"
                          output_path))) );
    ( "default output path workflow",
      fun () ->
        with_temp_file "onef-default-output-" ".1f" "если да то 1 иначе 0"
          (fun input_path ->
            let output_path = input_path ^ ".out" in
            Fun.protect
              ~finally:(fun () ->
                if Sys.file_exists output_path then Sys.remove output_path)
              (fun () ->
                match lex_file input_path with
                | Error message ->
                    fail_test "default output path workflow" message
                | Ok lexemes -> (
                    match dump_file output_path lexemes with
                    | Error message ->
                        fail_test "default output path workflow" message
                    | Ok () ->
                        expect_text_file "default output path workflow"
                          "[line: 1, char: 0-4] если\n\
                           [line: 1, char: 5-7] да\n\
                           [line: 1, char: 8-10] то\n\
                           [line: 1, char: 11-12] целое число 1\n\
                           [line: 1, char: 13-18] иначе\n\
                           [line: 1, char: 19-20] целое число 0"
                          output_path))) );
  ]

let () =
  List.iter
    (fun (name, test) ->
      try test ()
      with Failure message ->
        failwith (Format.sprintf "test failed: %s\n%s" name message))
    tests
