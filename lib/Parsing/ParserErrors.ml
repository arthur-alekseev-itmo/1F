open Lexing
open Ast

module ParserErrors = struct
  type range = Ast.range
  type t = { position: Ast.range; message: string }

  let fmt_json_range (range : Ast.pos * Ast.pos) =
    let p_start, p_end = range in
    let line_num = p_start.pos_lnum in
    let char_start = p_start.pos_cnum - p_start.pos_bol in
    let char_end = p_end.pos_cnum - p_end.pos_bol in
    let p_char c =
      Format.sprintf "{\"line\": %d, \"character\": %d}" line_num c
    in
    Format.sprintf "{\"start\": %s, \"end\": %s}" (p_char char_start)
      (p_char char_end)


  let fmt_json _ message (range : Ast.pos * Ast.pos) =
    let range_json = fmt_json_range range in
    Format.sprintf "{\"message\": \"%s\", \"range\": %s, \"severity\": 1}"
      message range_json


  let fmt_pos filename file message (position : Ast.pos * Ast.pos) =
    let p_start, p_end = position in
    let line_num = p_start.pos_lnum in
    let char_start, char_end =
      (p_start.pos_cnum - p_start.pos_bol, p_end.pos_cnum - p_end.pos_bol)
    in
    let lines = file |> String.split_on_char '\n' in
    let heading =
      Format.sprintf "File: %s, line: %d, characters %d-%d:" filename line_num
        char_start char_end
    in
    let content = List.nth lines (line_num - 1) in
    let underline =
      String.make char_start ' ' ^ String.make (char_end - char_start) '^'
    in
    Format.sprintf "%s\n%s\n%s\n%s" heading content underline message


  let fmt
      (filename : string) (file : string) (message : string) (position : range)
      =
    let eof_pos =
      { pos_fname= filename;
        pos_lnum= String.split_on_char '\n' file |> List.length |> ( + ) (-1);
        pos_bol= 0;
        pos_cnum= CCUtf8_string.of_string_exn file |> CCUtf8_string.n_chars
      }
    in
    match position with
    | Known (ps, pe) -> fmt_pos filename file message (ps, pe)
    | Eof ->
        fmt_pos filename file message
          (eof_pos, { eof_pos with pos_cnum= eof_pos.pos_cnum + 1 })
    | Unknown -> message ^ " (position unknown)"


  let print (filename : string) (file : string) (error : t) =
    let message = fmt filename file error.message error.position in
    Format.printf "%s" message
end
