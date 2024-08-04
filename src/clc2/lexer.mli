(** Tokenizer *)

type lex_error =
  | LEX_UNTERMINATED_STRING
  | LEX_UNTERMINATED_COMMENT
  | LEX_UNRECOGNIZED
  | LEX_UNKNOWN_ESCAPE of char

exception Lexer_error of Loc.loc * lex_error

val string_of_lex_error : lex_error -> string

val lex : string -> Lexing.lexbuf -> Parser.token

