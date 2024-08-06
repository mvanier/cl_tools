(** Intermediate representation 1. *)

(*

This IR is responsible for expanding out function applications
into a more explicit form.  Specifically, it makes the implicit
left-folding of applications explicit. So:

```
(a b c d e)
```

becomes:

```
((((a b) c) d) e)
```

*)

type id = string

type cmd = Ast.cmd

type expr =
  | Var   of id
  | Const of id
  | App   of expr * expr
[@@deriving sexp_of]

type form =
  | Def  of id * id list * expr   (* const * var * expr *)
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

val print : form -> unit

val convert : Ast.form -> form

