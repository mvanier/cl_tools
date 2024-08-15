(* lexer.mll *)

{

open Parser
open Lexing

type lex_error =
  | LEX_UNRECOGNIZED
  | LEX_UNTERMINATED_COMMENT

exception Lexer_error of lex_error

let string_of_lex_error = function
  | LEX_UNRECOGNIZED -> "unrecognized token"
  | LEX_UNTERMINATED_COMMENT -> "unterminated literate comment"

}

let whitespace = [' ' '\t' '\r']
let digits     = ['0' - '9']+

(*** Identifiers and identifier characters. ***)

let const_char = ['A' - 'Z']
let var_char   = ['a' - 'z']
let id_char    = ['A' - 'Z' 'a' - 'z' '0' - '9' '\'' '*']

let const = const_char id_char*
let var = var_char id_char*

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

  (* Integers. *)

  | digits as lxm { INT (int_of_string lxm) }

  (* Identifiers. *)

  | const as lxm { CONST lxm }

  | var as lxm { VAR lxm }

  (* Commands. *)

  | "#{"   { lex_literate_comment [] lexbuf }
  | "#nl"  { NEWLINE }
  | "#c"   { CURR }
  | "#cc"  { CURR2 }
  | "#ccc" { CURR3 }
  | "#q"   { QUIT }
  | "#n"   { NORM }
  | "#s"   { STEP }
  | "#sn"  { STEPN }
  | "#maxsteps" { MAXSTEPS }

  (* Literate comments. *)
  (* TODO *)

  (* Anything else is an error. *)

  | _ { raise (Lexer_error LEX_UNRECOGNIZED) }

and lex_literate_comment curr = parse
  | "}" { 
       let s = curr |> List.rev |> List.to_seq |> String.of_seq in
         LITERATE s
    }

  | '\n' {
      new_line lexbuf;
      lex_literate_comment ('\n' :: curr) lexbuf
    }

  | _ as lxm { lex_literate_comment (lxm :: curr) lexbuf }

  | eof { raise (Lexer_error LEX_UNTERMINATED_COMMENT) }

{
(* Nothing. *)
}
