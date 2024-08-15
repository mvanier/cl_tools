(* lexer.mll *)

{

open Parser
open Lexing

type lex_error =
  | LEX_UNRECOGNIZED
  | LEX_UNTERMINATED_COMMENT
  | LEX_INVALID_LOC

exception Lexer_error of lex_error

let lex_err err = raise (Lexer_error err)

let string_of_lex_error = function
  | LEX_UNRECOGNIZED -> "unrecognized token"
  | LEX_UNTERMINATED_COMMENT -> "unterminated literate comment"
  | LEX_INVALID_LOC -> "invalid location"

let validate_loc_string s =
  let len = String.length s in
    len > 1
    && s.[0] = ':'
    && String.for_all
         (fun c -> c = '0' || c = '1')
         (String.sub s 1 (len - 1))

let dirs_of_loc s : Ast.dir list =
  if not (validate_loc_string s) then
    lex_err LEX_INVALID_LOC;
  let s' = String.sub s 1 (String.length s - 1) in
  let chars = List.of_seq (String.to_seq s') in
    List.map
      (fun c -> if c = '0' then Ast.L else Ast.R)
      chars

}

let whitespace = [' ' '\t' '\r']
let digits     = ['0' - '9']+

(*** Identifiers and identifier characters. ***)

let const_char = ['A' - 'Z']
let var_char   = ['a' - 'z']
let id_char    = ['A' - 'Z' 'a' - 'z' '0' - '9' '\'' '*']
let loc        = ':' ['0' '1']+

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
  | "#p"   { PRINT_DEF }
  | "#c"   { CURR }
  | "#cc"  { CURR2 }
  | "#ccc" { CURR3 }
  | "#q"   { QUIT }
  | "#n"   { NORM }
  | "#s"   { STEP }
  | "#sn"  { STEPN }
  | "#sl"  { STEPL }
  | "#maxsteps" { MAXSTEPS }

  (* Location, for STEPL. *)
  | loc as lxm { LOC (dirs_of_loc lxm) }

  (* Anything else is an error. *)
  | _ { lex_err LEX_UNRECOGNIZED }

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

  | eof { lex_err LEX_UNTERMINATED_COMMENT }

{
(* Nothing. *)
}
