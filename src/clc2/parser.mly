%{

open Ast

%}

(* Punctuation. *)
%token LPAREN
%token RPAREN
%token SEMI
%token EQ

(* Keywords. *)
%token DEF

(* Commands. *)
%token NORM
%token STEP 
%token MAXSTEPS
%token QUIT

(* Identifiers. *)
%token <string> ID

(* Literals. *)
%token UNIT
%token <int> INT

%token EOI  (* end of interactive input *)

%start <Ast.form option> repl

%type <Ast.expr> expr
%type <Ast.cmd> cmd

%%

repl:
  | EOI { None }
  | f = form EOI { Some f }
  ;

form:
  | d = def  { d }
  | e = expr { Expr e }
  | c = cmd  { Cmd c }

def:
  | DEF; id = ID; LPAREN; args = list(ID); RPAREN; EQ; e = expr {
      Def (id, args, e)
    }

  | DEF; id = ID; EQ; e = expr {
      Def (id, [], e)
    }

expr:
  | id = ID { Atom id }

  | LPAREN; es = list(expr); RPAREN { List es }

cmd:
  | NORM { Norm }
  | STEP { Step }
  | MAXSTEPS; i = INT { MaxSteps i }
  | QUIT { Quit }
