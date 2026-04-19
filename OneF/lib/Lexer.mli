open Lexemes

module Lexer : sig 
(** Run lexical analysis of 1Ф Language on the given file *)
  val lex_file: string -> (Lexemes.t, string) result 
end
