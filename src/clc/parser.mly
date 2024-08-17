%{

open Ast

let rec parse_lambda_app es =
  match es with
    | []
    | [_] -> Utils.parse_err "parse_lambda_app: too few expressions"
    | [e1; e2] -> LApp (e1, e2)
    | e1 :: e2 :: es' -> parse_lambda_app (LApp (e1, e2) :: es')

let parse_lambda_lam vars l =
  List.fold_right (fun v e -> LLam (v, e)) vars l

%}

(* Punctuation. *)
%token LPAREN
%token RPAREN
%token SEMI
%token EQ
%token BS   (* backslash *)
%token DOT

(* Keywords. *)
%token DEF

(* Commands. *)
%token APPEND
%token CONVERT
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

(* Converter specifier for CONVERT. *)
%token <string> CONVERTER

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
%type <Ast.lambda> lexpr

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

lexpr:
  | id = VAR   { LVar id }
  | id = CONST { LConst id }
  | LPAREN; ls = list(lexpr); RPAREN { 
      parse_lambda_app ls
    }
  | LPAREN; BS; vars = list(VAR); DOT; l = lexpr; RPAREN { 
      parse_lambda_lam vars l
    }

lambda:
  | BS; vars = list(VAR); DOT; ls = nonempty_list(lexpr) {
      match ls with
        | [] -> failwith "this won't happen"
        | [l] -> parse_lambda_lam vars l 
        | _ -> parse_lambda_lam vars (parse_lambda_app ls)
    }

cmd:
  | APPEND; es = nonempty_list(expr) {
      Append es
    }

  | CONVERT; c = CONVERTER; l = lambda {
      match c with
        | ":ski"   -> Convert (SKI, l)
        | ":skibc" -> Convert (SKIBC, l)
        | ":bckw"  -> Convert (BCKW, l)
        | ":bckwi" -> Convert (BCKWI, l)
        | _ -> Utils.parse_err "invalid lambda converter name"
    }

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

