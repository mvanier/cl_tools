(* lexer.mll *)

{

open Lexer_utils
open Lexing

}

let whitespace = [' ' '\t' '\r']

let lower      = ['a' - 'z']
let upper      = ['A' - 'Z']
let var_chars  = ['a' - 'z' '0' - '9' '*' '\'']
let comb_chars = ['A' - 'Z' '0' - '9' '*' '\'']
let cmd_chars  = ['a' - 'z' '_']


let var  = lower var_chars*
let comb = upper comb_chars*
let cmd = cmd_chars+

rule lex filename = parse
  | eof { TOK_EOF }

  (* single-line comments *)
  | ";"[^'\n']*'\n'    { new_line lexbuf; lex filename lexbuf     }

  (* whitespace *)
  | whitespace+        { lex filename lexbuf                      }
  | '\n'               { new_line lexbuf; lex filename lexbuf     }

  (* reserved syntax tokens *)
  | '('                { TOK_LPAREN (make_loc filename lexbuf)    }
  | ')'                { TOK_RPAREN (make_loc filename lexbuf)    }
  | "def"              { TOK_DEF    (make_loc filename lexbuf)    }

  (* commands *)
  | ':' (cmd as lxm)   { TOK_CMD (make_loc filename lexbuf, lxm)  }

  (* non-negative integers; only used in commands *)
  | (['0' - '9']+ as lxm) {
      TOK_INT (make_loc filename lexbuf, int_of_string lxm)
    }

  (* primitive combinators *)
  | 'S'                { TOK_PRIM (make_loc filename lexbuf, "S") }
  | 'K'                { TOK_PRIM (make_loc filename lexbuf, "K") }
  | 'I'                { TOK_PRIM (make_loc filename lexbuf, "I") }
  | 'B'                { TOK_PRIM (make_loc filename lexbuf, "B") }
  | 'C'                { TOK_PRIM (make_loc filename lexbuf, "C") }
  | 'W'                { TOK_PRIM (make_loc filename lexbuf, "W") }

  (* non-primitive combinators *)
  | comb as lxm        { TOK_COMB (make_loc filename lexbuf, lxm) }

  (* variables *)
  | var as lxm         { TOK_VAR (make_loc filename lexbuf, lxm)  }

  | _ {
      raise (Lexer_error
               (make_loc filename lexbuf,
                LEX_UNRECOGNIZED))
    }

{
(* Nothing. *)
}
