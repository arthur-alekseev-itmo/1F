open Lexemes

module Lexer : sig
  val lex_file : string -> (Lexemes.t, string) result
  (** Run lexical analysis of 1Ф Language on the given file *)
end
