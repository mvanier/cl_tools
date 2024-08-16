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
%token DISPLAY_NORMAL
%token DISPLAY_RAW
%token <string> LITERATE
%token NEWLINE
%token PRINT_DEF
%token CURR
%token CURR2
%token CURR3
%token NORM
%token STEP 
%token STEPN
%token STEPL
%token MAXSTEPS
%token QUIT

(* Direction indicator, for STEPL. *)
%token <Ast.dir list> LOC

(* Identifiers. *)
%token <string> CONST
%token <string> VAR

(* Literals. *)
%token UNIT
%token <int> INT

%token EOI  (* end of interactive input *)

%start <Ast.form option> parse

%type <Ast.expr> expr
%type <Ast.cmd> cmd

%%

parse:
  | EOI { None }
  | f = form EOI { Some f }
  ;

form:
  | d = def  { d }
  | es = nonempty_list(expr) {
      match es with
        | [e] -> Expr e
        | _ -> Expr (List es)
    }
  | c = cmd  { Cmd c }

def:
  | DEF; id = CONST; args = nonempty_list(VAR);
    EQ; es = nonempty_list(expr) {
      match es with
        | [e] -> Def (id, args, e)
        | _ -> Def (id, args, List es)
    }

  | DEF; id = CONST; EQ; es = nonempty_list(expr) {
      match es with
        | [e] -> Def (id, [], e)
        | _ -> Def (id, [], List es)
    }

expr:
  | id = CONST { Const id }
  | id = VAR { Var id }

  | LPAREN; es = list(expr); RPAREN { List es }

cmd:
  | DISPLAY_NORMAL { Display Normal }

  | DISPLAY_RAW { Display Raw }

  | l = LITERATE { Literate l }

  | NEWLINE { Newline }

  | PRINT_DEF; cname = CONST; vars = list(VAR) {
      Print (cname, vars)
    }

  | CURR { Curr }

  | CURR2 { Curr2 }

  | CURR3 { Curr3 }

  | NORM { Norm }

  | STEP { Step }

  | STEPN; i = INT { StepN i }

  | STEPL; l = LOC { StepL l }

  | MAXSTEPS; i = INT { MaxSteps i }

  | QUIT { Quit }

