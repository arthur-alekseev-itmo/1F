open Parser
open Lexing

module ParserErrors = struct
  type range = Parser.range

  let fmt_pos file message (position: (Parser.pos * Parser.pos)) =
    let (p_start, p_end) = position in
    let line_num = p_start.pos_lnum in
    let (char_start, char_end) = p_start.pos_cnum - p_start.pos_bol, p_end.pos_cnum - p_end.pos_bol in
    let lines = file |> String.split_on_char '\n' in
    let heading = Format.sprintf "File: ??, line: %d, characters %d-%d:" line_num char_start char_end in
    let content = List.nth lines (line_num - 1) in
    let underline = String.make char_start ' ' ^ String.make (char_end - char_start) '^' in
    Format.sprintf "%s\n%s\n%s\n%s" heading content underline message

  let fmt (file: string) (message: string) (position: range) =
    let eof_pos = {
      pos_fname = "??";
      pos_lnum = String.split_on_char '\n' file |> List.length |> ((+) (-1));
      pos_bol = 0;
      pos_cnum = CCUtf8_string.of_string_exn file |> CCUtf8_string.n_chars
    } in
    match position with
    | Known (ps, pe) -> fmt_pos file message (ps, pe)
    | Eof -> fmt_pos file message (eof_pos, {eof_pos with pos_cnum = eof_pos.pos_cnum + 1})
    | Unknown -> message

  let print (file: string) (message: string) (position: range) =
    let message = fmt file message position in
    Format.printf "%s" message
end