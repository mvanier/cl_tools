(** Tokenizer *)

open Lexer_utils

val lex : string -> Lexing.lexbuf -> token

