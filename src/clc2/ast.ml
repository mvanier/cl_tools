open Sexplib.Conv
open Pprint

type id = string
[@@deriving sexp_of]

type cmd =
  | Norm
  | Step
  | MaxSteps of int
  | Quit
[@@deriving sexp_of]

type expr =
  | Atom of id
  | List of expr list
[@@deriving sexp_of]

type form =
  | Def  of id * id list * expr
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

let print form = 
  sexp_of_form form |> print_sexp
