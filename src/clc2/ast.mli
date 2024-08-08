(* Abstract syntax tree. *)

open Utils

type cmd =
  | Step
  | Norm
  | MaxSteps of int
  | Quit
[@@deriving sexp_of]

type expr =
  | Var   of id
  | Const of id
  | List  of expr list
[@@deriving sexp_of]

type form =
  | Def  of id * id list * expr   (* const * var * expr *)
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

val print : form -> unit
