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

exception Parse_error of string

type id = string

type cmd = Ast.cmd

type expr =
  | Var   of id
  | Const of id
  | App   of expr * expr
[@@deriving sexp_of]

type dexpr =
  | DVar of int
  | DConst of id
  | DApp of dexpr * dexpr

type form =
  | Def  of int * dexpr
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

val print : form -> unit

val convert : Ir.form -> form

