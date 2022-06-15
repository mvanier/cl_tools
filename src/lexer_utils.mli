(** Lexer types and utility functions. *)

type token =
  | TOK_EOF
  | TOK_LPAREN of Loc.loc
  | TOK_RPAREN of Loc.loc
  | TOK_PRIM   of Loc.loc * string
  | TOK_COMB   of Loc.loc * string
  | TOK_VAR    of Loc.loc * string
  | TOK_DEF    of Loc.loc
  | TOK_PRAGMA of Loc.loc * string

type lex_error =
  | LEX_UNTERMINATED_STRING
  | LEX_UNRECOGNIZED
  | LEX_UNKNOWN_ESCAPE of char

exception Lexer_error of Loc.loc * lex_error

val string_of_lex_error : lex_error -> string

val string_of_token      : token -> string
val string_of_token_loc  : token -> string
val make_loc             : string -> Lexing.lexbuf -> Loc.loc
val loc_of_token         : token -> Loc.loc
val token_eq             : token -> token -> bool
val token_eq_constructor : token -> token -> bool
val lex_escape           : Loc.loc -> char -> string
