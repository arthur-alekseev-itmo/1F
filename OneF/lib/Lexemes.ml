module Lexemes = struct
  type t =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | BoolLiteral of bool
    | Let
    | Rec
    | In
    | Arrow
    | Operator of string
    | Wildcard
    | Identifier of string
    | Lambda
    | Type
    | Eof
    | LCbr
    | RCbr
    | LBr
    | RBr
    | LPar
    | RPar
    | If
    | Then
    | Else
    | VBar
    | Semicolon
    | Of
    | Dot
    | Comma
  [@@deriving show]

  let to_string = function
    | IntLiteral i -> Format.sprintf "целое число %d" i
    | FloatLiteral f -> Format.sprintf "реальное число %f" f
    | StringLiteral s -> Format.sprintf "строчка \"%s\"" s
    | BoolLiteral true -> "да"
    | BoolLiteral false -> "нет"
    | Let -> "пусть"
    | Rec -> "рек"
    | In -> "в"
    | Arrow -> "стрелка"
    | Operator o -> Format.sprintf "оператор %s" o
    | Wildcard -> "дикая карта"
    | Identifier p -> Format.sprintf "имя %s" p
    | Lambda -> "лямбда"
    | Type -> "тип"
    | Eof -> "конец файла"
    | LPar -> "скобка ("
    | RPar -> "скобка )"
    | LBr -> "скобка ["
    | RBr -> "скобка ]"
    | LCbr -> "скобка {"
    | RCbr -> "скобка }"
    | If -> "если"
    | Then -> "то"
    | Else -> "иначе"
    | VBar -> "палочка |"
    | Semicolon -> "точка с запятой"
    | Of -> "из"
    | Dot -> "точка"
    | Comma -> "запятая"

  let write_file (path : string) (content : string) =
    try
      Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc content)
      |> Result.ok
    with Sys_error x -> Result.error @@ Format.sprintf "Failed writing to a file: %s" x

  let t_with_pos_to_string ((token, s, e) : t * Lexing.position * Lexing.position) =
    Format.sprintf "[line: %d, char: %d-%d] %s" s.pos_lnum (s.pos_cnum - s.pos_bol)
      (e.pos_cnum - e.pos_bol) (to_string token)

  let dump (path : string) lst =
    let string = List.map t_with_pos_to_string lst |> String.concat "\n" in
    write_file path string
end
