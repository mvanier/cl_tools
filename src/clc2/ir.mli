(* Intermediate representation 1. *)

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

val convert : Ast.form -> form

val print : form -> unit
