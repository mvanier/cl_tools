(* Abstract syntax tree. *)

open Utils

type cmd =
  | Literate of string  (* print a literate comment *)
  | Newline             (* print a newline *)
  | Curr                (* print current expression *)
  | Curr2               (* print current raw expression *)
  | Step                (* evaluate one step *)
  | StepN of int        (* evaluate N steps *)
  | Norm                (* normalize the current expression *)
  | MaxSteps of int     (* set the maximum number of reduction steps *)
  | Quit                (* exit the interpreter *)
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
