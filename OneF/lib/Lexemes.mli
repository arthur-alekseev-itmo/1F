module Lexemes : sig
  type lexeme_payload =
    | IntLiteral of string  (** Integer literal, i.e. "3241340597", "-1234", "0" *)
    | FloatLiteral of string  (** Float literal, i.e. "0.32", "0.3241340597", "-77137." *)
    | StringLiteral of string  (** String literal, i.e. ""Hello world!"", ""\n\n\nYo"", """"*)
    | BoolLiteral of string  (** Boolean literal, i.e. "true" or "false" *)
    | Let
    | RecFlag
    | In  (** Let expression, syntax is "пусть <имя> = <выраж> в <выраж>" *)
    | Operator of string  (** Operator, that is any combination of characters: +-*/=@.*)
    | Wildcard  (** The "_" symbol*)
    | Identifier of string  (** Identifier, i.e. "окэмл_про", "ф1", "_33"*)
    | Fun  (** Lambda declaration, syntax is "лямба <паттерн> -> <выражение>"*)
    | Type  (** Type declaration *)
    | Eof  (** End of file *)
    | LCbr
    | RCbr  (** "{", "}" *)
    | LBr
    | RBr  (** "[", "]" *)
    | LPar
    | RPar  (** "(", ")" *)
    | If
    | Then
    | Else  (** ITE expression, syntax is "если <выражение> то <выражение> иначе <выражение>" *)
    | VBar
    | Semicolon  (** ";" *)
    | Of  (** "of" keyword, for type declaration *)
    | Dot
    | Comma  (** ".", "," *)
  [@@deriving show]

  type lexeme_position = { line : int; start : int; length : int; file : string } [@@deriving show]
  type lexeme = { payload : lexeme_payload; position : lexeme_position } [@@deriving show]
  type t = lexeme list

  val dump_file : string -> t -> (unit, string) result
  (** Dump the lexemes parsed into a file *)
end
