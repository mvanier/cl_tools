open Lexing
open Loc

type token =
  | TOK_EOF
  | TOK_LPAREN of Loc.loc
  | TOK_RPAREN of Loc.loc
  | TOK_PRIM   of Loc.loc * string
  | TOK_COMB   of Loc.loc * string
  | TOK_VAR    of Loc.loc * string
  | TOK_DEF    of Loc.loc
  | TOK_CMD of Loc.loc * string

type lex_error =
  | LEX_UNTERMINATED_STRING
  | LEX_UNRECOGNIZED
  | LEX_UNKNOWN_ESCAPE of char

exception Lexer_error of loc * lex_error

let string_of_lex_error = function
  | LEX_UNTERMINATED_STRING -> "unterminated string"
  | LEX_UNRECOGNIZED        -> "unrecognized token"
  | LEX_UNKNOWN_ESCAPE c    ->
    Printf.sprintf "unknown escape sequence: \\%c" c

let string_of_token = function
  | TOK_EOF            -> "TOK_EOF"
  | TOK_LPAREN  _      -> "TOK_LPAREN"
  | TOK_RPAREN  _      -> "TOK_RPAREN"
  | TOK_PRIM   (_, id) -> "TOK_PRIM " ^ id
  | TOK_COMB   (_, id) -> "TOK_COMB " ^ id
  | TOK_VAR    (_, id) -> "TOK_VAR " ^ id
  | TOK_DEF     _      -> "TOK_DEF"
  | TOK_CMD (_, s)  -> "TOK_CMD[" ^ s ^ "]"

let string_of_token_loc tok =
  let sol = string_of_loc_short in
    match tok with
      | TOK_EOF            -> "TOK_EOF"
      | TOK_LPAREN  l      -> "TOK_LPAREN   (" ^ sol l ^ ")"
      | TOK_RPAREN  l      -> "TOK_RPAREN   (" ^ sol l ^ ")"
      | TOK_PRIM   (l, id) ->
        Printf.sprintf "TOK_PRIM[%s]  (%s)" id (sol l)
      | TOK_COMB   (l, id) ->
        Printf.sprintf "TOK_COMB[%s]  (%s)" id (sol l)
      | TOK_VAR    (l, id) ->
        Printf.sprintf "TOK_VAR %s   (%s)" id (sol l)
      | TOK_DEF     l      -> "TOK_DEF   (" ^ sol l ^ ")"
      | TOK_CMD (l, id) ->
        Printf.sprintf "TOK_CMD[%s]  (%s)" id (sol l)

let make_loc filename lexbuf =
  get_loc filename (lexeme_start_p lexbuf) (lexeme lexbuf)

(* Special fake location for EOF. *)
let eof_loc = {
  source_name = "EOF";
  start_line  = 0;
  start_char  = 0;
  end_line    = 0;
  end_char    = 0
}

let loc_of_token = function
  | TOK_EOF -> eof_loc
  | TOK_LPAREN       l
  | TOK_RPAREN       l
  | TOK_PRIM        (l, _)
  | TOK_COMB        (l, _)
  | TOK_VAR         (l, _)
  | TOK_DEF          l
  | TOK_CMD      (l, _) -> l

let token_eq token1 token2 =
  match (token1, token2) with
    | (TOK_EOF, TOK_EOF) -> true
    | (TOK_LPAREN  l1,      TOK_LPAREN  l2)
    | (TOK_RPAREN  l1,      TOK_RPAREN  l2)      -> loc_eq l1 l2
    | (TOK_PRIM   (l1, i1), TOK_PRIM   (l2, i2)) -> loc_eq l1 l2 && i1 = i2
    | (TOK_COMB   (l1, i1), TOK_COMB   (l2, i2)) -> loc_eq l1 l2 && i1 = i2
    | (TOK_VAR    (l1, i1), TOK_VAR    (l2, i2)) -> loc_eq l1 l2 && i1 = i2
    | (TOK_DEF     l1,      TOK_DEF     l2)      -> loc_eq l1 l2
    | (TOK_CMD (l1, s1), TOK_CMD (l2, s2)) -> loc_eq l1 l2 && s1 = s2
    | _ -> false

let token_eq_constructor token1 token2 =
  match (token1, token2) with
    | (TOK_EOF,      TOK_EOF     )
    | (TOK_LPAREN _, TOK_LPAREN _)
    | (TOK_RPAREN _, TOK_RPAREN _)
    | (TOK_PRIM   _, TOK_PRIM   _)
    | (TOK_COMB   _, TOK_COMB   _)
    | (TOK_VAR    _, TOK_VAR    _)
    | (TOK_DEF    _, TOK_DEF    _)
    | (TOK_CMD _, TOK_CMD _) -> true
    | _ -> false

let lex_escape loc = function
  | 'n'  -> "\n"
  | 't'  -> "\t"
  | '\\' -> "\\"
  | _ as lxm ->
    begin
      Printf.printf "ESCAPE CHAR: [%c]\n%!" lxm;
      raise (Lexer_error (loc, LEX_UNKNOWN_ESCAPE lxm))
    end
