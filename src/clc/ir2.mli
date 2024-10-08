(** Intermediate representation 2. *)

(*

This IR is responsible for changing the representation of definitions
to a numeric form, reminiscent of (but distinct from) de Bruijn indices.
In particular, variables are replaced by integers.
This makes evaluation a bit simpler and avoids all possible problems
with variable capture (though I don't think you would have any 
such problems even without this pass).

```
def S x y z = ((x z) (y z))
```

becomes (schematically):

```
def S = (3, [[0 2] [1 2]]);
```

*)

open Utils

type cmd = Ast.cmd
[@@deriving sexp_of]

type expr =
  | Var   of id
  | Const of id
  | App   of expr * expr
[@@deriving sexp_of]

type dexpr =
  | DVar   of int
  | DConst of id
  | DApp   of dexpr * dexpr
[@@deriving sexp_of]

type comb =
  {
    arity : int;
    body : dexpr;
  }
[@@deriving sexp_of]

type form =
  | Def  of id * comb
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

(** Variable that indicates the display mode (raw or normal). *)
val display_mode : Ast.disp ref

(** Prefix used to print expressions. *)
val print_prefix : string

(** Print an expression. *)
val print_expr : expr -> unit

(** Pretty-print an expression, for REPL output. Return the string. *)
val spprint_expr : expr -> string

(** Pretty-print an expression, for REPL output. Add an optional prefix. *)
val pprint_expr : ?prefix:string -> expr -> unit

(** Pretty-print an expression, for REPL output.
    This prints all applications explicitly.
    Add an optional prefix. *)
val pprint_expr2 : ?prefix:string -> expr -> unit

(** Print a form. *)
val print : form -> unit

(** Convert an IR expr to an expr. *)
val convert_expr : Ir.expr -> expr

(** Convert an IR form to a form. *)
val convert : Ir.form -> form

