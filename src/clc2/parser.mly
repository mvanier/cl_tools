%{

open Utils
open Ast

type binding = id * expr

type bindings = binding list * expr list

%}

(* Punctuation. *)
%token LPAREN
%token RPAREN
%token SEMI
%token EQ

(* Keywords. *)
%token DEF

(* Identifiers. *)
%token <Utils.sym> ID

(* Literals. *)
%token UNIT
%token <int> INT

%token EOF  (* end of file *)
%token EOI  (* end of interactive input *)

(* There are two entry points.
   One (`repl`) is for entering code in a REPL,
   and each entered form needs to end in `;;` (the EOI token).
   The other (`file`) is for a file of code; the EOI token is _not_ used,
   but top-level expressions need to be surrounded by curly braces. *)

%start <Ast.def list> file
%start <Ast.def option> repl

%type <Ast.def> def_repl
%type <Ast.def> def_file
%type <Ast.def> def_not_expr
%type <Ast.expr> expr
%type <Utils.id list> arglist
%type <binding>binding
%type <bindings>binding_form
%type <Ast.expr * Ast.expr list>cond_binding
%type <Ast.expr list> exprlist_comma
%type <Ast.expr list> exprlist1_comma
%type <Ast.expr list> exprlist_semi
%type <Loc.loc * Utils.const> lit


%%

file:
  | ds = list(def_file) EOF { ds }

repl:
  | EOI { None }
  | EOF { None }
  | d = def_repl EOI { Some d }
  ;

def_file:
  | d = def_not_expr { d }
  | LBRACE e = expr RBRACE {
      let l = loc_of_expr e in Exp (l, e)
    }

def_repl:
  | d = def_not_expr { d }
  | e = expr {
      let l = loc_of_expr e in Exp (l, e)
    }

def_not_expr:
  | l = VAL; id = ID; LBRACE; e = expr; RBRACE {
      let (_, id) = id in
        Val (l, id, e)
    }

  | l = DEF; id = ID; LPAREN; args = arglist; RPAREN;
      LBRACE; es = exprlist_semi; RBRACE {
      let (_, id) = id in
        Def (l, id, args, es)
    }

  | l = DEF; OP; LPAREN; op = INFIXOP0; RPAREN;
          LPAREN; args = arglist; RPAREN; LBRACE; es = separated_list(SEMI, expr); RBRACE
  | l = DEF; OP; LPAREN; op = INFIXOP1; RPAREN;
          LPAREN; args = arglist; RPAREN; LBRACE; es = separated_list(SEMI, expr); RBRACE
  | l = DEF; OP; LPAREN; op = INFIXOP2; RPAREN;
          LPAREN; args = arglist; RPAREN; LBRACE; es = separated_list(SEMI, expr); RBRACE
  | l = DEF; OP; LPAREN; op = INFIXOP3; RPAREN;
          LPAREN; args = arglist; RPAREN; LBRACE; es = separated_list(SEMI, expr); RBRACE
  | l = DEF; OP; LPAREN; op = UNARYOP; RPAREN;
          LPAREN; args = arglist; RPAREN; LBRACE; es = separated_list(SEMI, expr); RBRACE {
      let (_, op) = op in
        Def (l, op, args, es)
    }

  | l = VALREC; LBRACE; bs = separated_list(SEMI, binding); RBRACE {
      ValRec (l, bs)
    }

  | l = USE; LPAREN; s = STR; RPAREN {
      let (_, s) = s in
        Use (l, s)
    }

expr:
  | v = lit { let (l, v) = v in Literal (l, v) }

  | id = ID { let (l, id) = id in Var (l, id) }

  | e1 = expr; OROP;  e2 = expr {
      let l = loc_of_expr e1 in
        Or (l, e1, e2)
  }

  | e1 = expr; ANDOP; e2 = expr {
      let l = loc_of_expr e1 in
        And (l, e1, e2)
  }

  | e1 = expr; op = INFIXOP0; e2 = expr
  | e1 = expr; op = INFIXOP1; e2 = expr
  | e1 = expr; op = INFIXOP2; e2 = expr
  | e1 = expr; op = INFIXOP3; e2 = expr {
      let l = loc_of_expr e1 in
      let (lop, op) = op in
        Call (l, Var (lop, op), [e1; e2])
    }

  | op = UNARYOP; e = expr {
      let (l, op) = op in
        Call (l, Var (l, op), [e])
    }

  | LPAREN; e = expr; RPAREN { e }

  (* Convert an operator into an expression. *)

  | OP; LPAREN; op = INFIXOP0; RPAREN
  | OP; LPAREN; op = INFIXOP1; RPAREN
  | OP; LPAREN; op = INFIXOP2; RPAREN
  | OP; LPAREN; op = INFIXOP3; RPAREN
  | OP; LPAREN; op = UNARYOP; RPAREN {
      let (lop, op) = op in Var (lop, op)
    }

  (* Function call when the function is an identifier. *)

  | fname = ID; LPAREN; exprs = exprlist_comma; RPAREN {
      let (l, fname) = fname in
        Call (l, Var (l, fname), exprs)
    }

  (* Function call when the function is an arbitrary expression. *)

  | l = CALL; LPAREN; exprs = exprlist1_comma; RPAREN {
      match exprs with
        | [] -> failwith "invalid `call` form"  (* should never happen *)
        | func :: args -> Call (l, func, args)
  }

  (* Anonymous function. *)

  | l = FUN; LPAREN; args = arglist; RPAREN
      LBRACE; es = separated_list(SEMI, expr); RBRACE {
        Fun (l, args, es)
    }

  (* Conditional (if/else). *)

  | l = IF; LPAREN; test = expr; RPAREN;
      LBRACE; then_body = expr; RBRACE;
      ELSE; LBRACE; else_body = expr; RBRACE {
      If (l, test, then_body, else_body)
    }

  (* Conditional (cond). *)
  | l = COND; LBRACE; cs = separated_list(SEMI, cond_binding); RBRACE {
      Cond (l, cs)
    }

  (* Mutation (set). *)

  | id = ID; COLONEQ; e = expr {
      let (l, id) = id in
        Set (l, id, e)
    }

  (* Sequencing (do). *)

  | l = DO; LBRACE; exprs = exprlist_semi; RBRACE {
      Do (l, exprs)
    }

  (* Lists. *)

  | l = LBRACK; exprs = separated_list(COMMA, expr); RBRACK {
      List (l, exprs)
    }

  (* Local bindings (let, let*, letrec). *)

  | l = LET; b = binding_form {
      let (bs, e) = b in Let (l, bs, e)
    }

  | l = LETSTAR; b = binding_form {
      let (bs, e) = b in LetStar (l, bs, e)
    }

  | l = LETREC; b = binding_form {
      let (bs, es) = b in LetRec (l, bs, es)
    }

  (* Exception handling (throw, try, catch). *)

  | l = THROW; LPAREN; e = expr; RPAREN {
      Throw (l, e)
    }

  | l = TRY; LBRACE; try_exprs = exprlist_semi; RBRACE;
        CATCH; LPAREN; id = ID; RPAREN;
        LBRACE; catch_exprs = exprlist_semi; RBRACE {
      let (_, id) = id in
        TryCatch (l, try_exprs, id, catch_exprs)
    }

arglist:
  | ids = separated_list(COMMA, ID) {
      List.map snd ids
    }

binding_form:
  | LBRACE; bs = separated_list(SEMI, binding); RBRACE;
      IN; LBRACE; es = exprlist_semi; RBRACE {
      (bs, es)
    }

binding:
  | id = ID; EQ; e = expr {
      let (_, id) = id in (id, e)
    }

cond_binding:
  | LPAREN; e = expr; RPAREN;
    LBRACE; es = exprlist_semi; RBRACE {
      (e, es)
    }

exprlist_comma:
  | exprs = separated_list(COMMA, expr) {
      exprs
    }

exprlist1_comma:
  | exprs = separated_nonempty_list(COMMA, expr) {
      exprs
    }

exprlist_semi:
  | exprs = separated_list(SEMI, expr) {
      exprs
    }

lit:
  | u = UNIT  { (u, Unit) }
  | b = BOOL  { (fst b, Bool  (snd b)) }
  | i = INT   { (fst i, Int   (snd i)) }
  | q = RAT   { (fst q, Rat   (snd q)) }
  | f = FLOAT { (fst f, Float (snd f)) }
  | s = STR   { (fst s, Str   (snd s)) }
  | s = SYM   { (fst s, Sym   (snd s)) }

