(* Abstract syntax tree. *)

type id = string

type cmd =
  | Norm
  | Step
  | MaxSteps of int
  | Quit

type expr =
  | Atom of id
  | List of expr list

type form =
  | Def  of id * id list * expr
  | Expr of expr
  | Cmd  of cmd

