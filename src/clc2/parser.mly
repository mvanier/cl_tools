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
%token <string> CONST
%token <string> VAR

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
  | es = list(expr) { Expr (List es) }
  | c = cmd  { Cmd c }

def:
  | DEF; id = CONST; args = list(VAR); EQ; es = list(expr) {
      Def (id, args, List es)
    }

  | DEF; id = CONST; EQ; e = expr {
      Def (id, [], e)
    }

expr:
  | id = CONST { Const id }
  | id = VAR { Var id }

  | LPAREN; es = list(expr); RPAREN { List es }

cmd:
  | NORM { Norm }
  | STEP { Step }
  | MAXSTEPS; i = INT { MaxSteps i }
  | QUIT { Quit }

