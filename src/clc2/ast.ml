open Sexplib.Conv
open Pprint
open Utils

type cmd =
  | Literate of string
  | Newline
  | Print of id * id list
  | Curr
  | Curr2
  | Curr3
  | Step
  | StepN of int
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
  | Def  of id * id list * expr
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

let print form = 
  sexp_of_form form |> print_sexp
