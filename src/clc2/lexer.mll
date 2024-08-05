(* lexer.mll *)

{

open Parser
open Lexing

type lex_error = LEX_UNRECOGNIZED

exception Lexer_error of lex_error

let string_of_lex_error = function
  | LEX_UNRECOGNIZED -> "unrecognized token"

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
  ['A' - 'Z' 'a' - 'z' '0' - '9' '\'' '*']

let id = id_char+

rule lex filename = parse
  | eof { raise End_of_file }
  | ";" { EOI }

  (* single-line comments *)
  | "//"[^'\n']*'\n' { new_line lexbuf; lex filename lexbuf }

  (* whitespace *)
  | whitespace+ { lex filename lexbuf }
  | '\n'        { new_line lexbuf; lex filename lexbuf }

  (* Reserved syntax tokens. *)
  | '('  { LPAREN }
  | ')'  { RPAREN }

  (* Keywords. *)

  | "def" { DEF }

  | "=" { EQ }

  (* integers *)

  | digits as lxm { INT (int_of_string lxm) }

  (* identifiers *)

  | id as lxm { ID lxm }

  (* Anything else is an error. *)

  | _ { raise (Lexer_error LEX_UNRECOGNIZED) }

{
(* Nothing. *)
}
