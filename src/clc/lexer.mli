(** Tokenizer *)

type lex_error = 
  | LEX_UNRECOGNIZED
  | LEX_UNTERMINATED_COMMENT
  | LEX_INVALID_LOC
  
exception Lexer_error of lex_error

val string_of_lex_error : lex_error -> string

val lex : string -> Lexing.lexbuf -> Parser.token

