(* lexer.mll *)

{

open Loc
open Parser
open Lexing
module S = Symbol

type lex_error =
  | LEX_UNTERMINATED_STRING
  | LEX_UNTERMINATED_COMMENT
  | LEX_UNRECOGNIZED
  | LEX_UNKNOWN_ESCAPE of char

exception Lexer_error of loc * lex_error

let string_of_lex_error = function
  | LEX_UNTERMINATED_STRING  -> "unterminated string"
  | LEX_UNTERMINATED_COMMENT -> "unterminated comment"
  | LEX_UNRECOGNIZED         -> "unrecognized token"
  | LEX_UNKNOWN_ESCAPE c ->
      Printf.sprintf "unknown escape sequence: \\%c" c

let lex_escape loc = function
  | 'n'  -> "\n"
  | 't'  -> "\t"
  | '\\' -> "\\"
  | '"'  -> "\""
  | _ as lxm ->
    begin
      Printf.printf "ESCAPE CHAR: [%c]\n%!" lxm;
      raise (Lexer_error (loc, LEX_UNKNOWN_ESCAPE lxm))
    end

let make_loc filename lexbuf =
  get_loc filename (lexeme_start_p lexbuf) (lexeme lexbuf)

}

let whitespace = [' ' '\t' '\r']
let digit      = ['0' - '9']+
let digits     = digit+
let hexdigit   = ['0' - '9' 'a' - 'f']
let octdigit   = ['0' - '7']
let bindigit   = ['0' - '1']
let sign       = ['+' '-']?
let exp        = ['e' 'E'] sign digits
let floating   = digits '.' digits exp?

(*** Identifiers and identifier characters. ***)

let id_char =
  ['A' - 'Z' 'a' - 'z' '0' - '9' '_' '?']

let op_char =
  ['!' '#' '$' '%' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|' '~']

let id = id_char+

rule lex filename = parse
  | eof  { EOF }
  | ";;" { EOI }

  (* single-line comments *)
  | "//"[^'\n']*'\n' { new_line lexbuf; lex filename lexbuf }

  (* multiline comments *)
  | "/*" { lex_multiline_comment filename 0 lexbuf }

  (* whitespace *)
  | whitespace+ { lex filename lexbuf }
  | '\n'        { new_line lexbuf; lex filename lexbuf }

  (* Reserved syntax tokens. *)
  | '('  { LPAREN  (make_loc filename lexbuf) }
  | ')'  { RPAREN  (make_loc filename lexbuf) }
  | '['  { LBRACK  (make_loc filename lexbuf) }
  | ']'  { RBRACK  (make_loc filename lexbuf) }
  | '{'  { LBRACE  (make_loc filename lexbuf) }
  | '}'  { RBRACE  (make_loc filename lexbuf) }
  | ','  { COMMA   (make_loc filename lexbuf) }
  | ';'  { SEMI    (make_loc filename lexbuf) }

  (* Keywords. *)

  | "call" {
      CALL (make_loc filename lexbuf)
    }

  | "catch" {
      CATCH (make_loc filename lexbuf)
    }

  | "cond" {
      COND (make_loc filename lexbuf)
    }

  | "def" {
      DEF (make_loc filename lexbuf)
    }

  | "do" {
      DO (make_loc filename lexbuf)
    }

  | "else" {
      ELSE (make_loc filename lexbuf)
    }

  | "fun" {
      FUN (make_loc filename lexbuf)
    }

  | "if" {
      IF (make_loc filename lexbuf)
    }

  | "in" {
      IN (make_loc filename lexbuf)
    }

  | "let" {
      LET (make_loc filename lexbuf)
    }

  | "let*" {
      LETSTAR (make_loc filename lexbuf)
    }

  | "letrec" {
      LETREC (make_loc filename lexbuf)
    }

  | "op" {
      OP (make_loc filename lexbuf)
    }

  | "throw" {
      THROW (make_loc filename lexbuf)
    }

  | "try" {
      TRY (make_loc filename lexbuf)
    }

  | "use" {
      USE (make_loc filename lexbuf)
    }

  | "val" {
      VAL (make_loc filename lexbuf)
    }

  | "valrec" {
      VALREC (make_loc filename lexbuf)
    }

  | "=" {
      EQ (make_loc filename lexbuf)
    }

  | ":=" {
      COLONEQ (make_loc filename lexbuf)
    }

  (* unit *)
  | "#u" {
      UNIT (make_loc filename lexbuf)
    }

  (* booleans *)
  | "#f" {
      BOOL (make_loc filename lexbuf, false)
    }

  | "#t" {
      BOOL (make_loc filename lexbuf, true)
    }

  (* integers *)

  | digits as lxm {
      INT (make_loc filename lexbuf, Z.of_string lxm)
    }

  (* rationals *)

  | (digits as num) '/' (digits as den) {
      let loc = make_loc filename lexbuf in
      let qs  = num ^ "/" ^ den in
      let q   = Q.of_string qs in
        RAT (loc, q)
    }

  (* floats *)

  | floating as lxm {
      FLOAT (make_loc filename lexbuf, float_of_string lxm)
    }

  (* strings *)

  | '"' {
      let loc = make_loc filename lexbuf in
      let s = lex_string loc "" lexbuf in
        STR (loc, s)
    }

  (* symbols *)

  | ':' (id as lxm) {
      SYM (make_loc filename lexbuf, S.of_string lxm)
    }

  (* identifiers *)

  | id as lxm {
      ID (make_loc filename lexbuf, S.of_string lxm)
    }

  (* operators *)

  | "||" {
      let loc = make_loc filename lexbuf in
        OROP loc
  }

  | "&&" {
      let loc = make_loc filename lexbuf in
        ANDOP loc
  }

  | ('<' op_char*) as lxm
  | ('>' op_char*) as lxm
  | ('=' op_char*) as lxm
  | ('!' op_char*) as lxm {
      let loc = make_loc filename lexbuf in
      let s = S.of_string lxm in
        INFIXOP0 (loc, s)
    }

  | ('+' op_char*) as lxm
  | ('-' op_char*) as lxm {
      let loc = make_loc filename lexbuf in
      let s = S.of_string lxm in
        INFIXOP1 (loc, s)
    }

  | ('*' op_char*) as lxm
  | ('/' op_char*) as lxm {
      let loc = make_loc filename lexbuf in
      let s = S.of_string lxm in
        INFIXOP2 (loc, s)
    }

  | ('^' op_char*) as lxm {
      let loc = make_loc filename lexbuf in
      let s = S.of_string lxm in
        INFIXOP3 (loc, s)
    }

  | ('~' op_char*) as lxm {
      let loc = make_loc filename lexbuf in
      let s = S.of_string lxm in
        UNARYOP (loc, s)
    }

  (* Anything else is an error. *)

  | _ {
      raise (Lexer_error (make_loc filename lexbuf, LEX_UNRECOGNIZED))
    }

and lex_string loc s = parse
  | '"' { s }

  | '\n' {
      new_line lexbuf;
      lex_string loc (s ^ "\n") lexbuf
    }

  | '\\' (_ as lxm) {
      let c = lex_escape loc lxm in
        lex_string loc (s ^ c) lexbuf
    }

  | [^ '\n' '"' '\\']+ as lxm {
      lex_string loc (s ^ lxm) lexbuf
    }

  | eof {
      raise (Lexer_error (loc, LEX_UNTERMINATED_STRING))
    }

and lex_multiline_comment filename i = parse
  | "*/" {
      if i = 0 then
        lex filename lexbuf
      else
        lex_multiline_comment filename (i - 1) lexbuf
    }

  | "/*" {
      lex_multiline_comment filename (i + 1) lexbuf
    }

  | '\n' {
      new_line lexbuf;
      lex_multiline_comment filename i lexbuf
    }

  | _ {
      lex_multiline_comment filename i lexbuf
    }

  | eof {
      raise (Lexer_error (make_loc filename lexbuf, LEX_UNTERMINATED_COMMENT))
    }

{
(* Nothing. *)
}
