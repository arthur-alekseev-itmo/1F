module Lexer : sig
  type lexeme_payload =
    (** Integer literal, i.e. "3241340597", "-1234", "0" *)
    | IntLiteral of string
    (** Float literal, i.e. "0.32", "0.3241340597", "-77137." *)
    | FloatLiteral of string
    (** String literal, i.e. ""Hello world!"", ""\n\n\nYo"", """"*)
    | StringLiteral of string
    (** Boolean literal, i.e. "true" or "false" *)
    | BoolLiteral of string
    (** Let expression, syntax is "пусть <имя> = <выражение> в <выражение>" *)
    | Let | RecFlag | In
    (** Operator, that is any combination of characters: +-*/=@.*)
    | Operator of string
    (** Identifier, i.e. "окэмл_про", "ф1", "_33", starts with А-Я/а-я/_*)
    | Identifier of string
    (** The "_" symbol*)
    | Wildcard
    (** Lambda declaration, syntax is "лямба <паттерн> -> <выражение>"*)
    | Fun
    (** ITE expression, syntax is "если <выражение> то <выражение> иначе <выражение>" *)
    | If | Then | Else
    (** "(", ")" *)
    | LPar | RPar
    (** "[", "]" *)
    | LBr | RBr
    (** "{", "}" *)
    | LCbr | RCbr
    (** End of file *)
    | Eof
    (** Type declaration *)
    | Type
    (** "|" *)
    | VBar
    (** "of" keyword, for type declaration *)
    | Of
    (** ";" *)
    | Semicolon
    (** ".", "," *)
    | Dot | Comma

  type lexeme_position = {
    line: int;
    start: int;
    length: int;
    file: string;
  }

  type lexeme = {
    payload: lexeme_payload;
    position: lexeme_position;
  }
end


