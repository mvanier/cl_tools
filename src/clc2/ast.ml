open Sexplib.Conv
open Pprint
open Utils

type dir = L | R
[@@deriving sexp_of]

type disp =
  | Normal
  | Raw
[@@deriving sexp_of]

type converter =
  | SKI
  | SKIBC
  | BCKW
[@@deriving sexp_of]

type lambda =
  | LVar   of id
  | LConst of id
  | LApp   of lambda * lambda
  | LLam   of id * lambda
[@@deriving sexp_of]

type expr =
  | Var   of id
  | Const of id
  | List  of expr list
[@@deriving sexp_of]

type cmd =
  | Append   of expr list
  | Convert  of converter * lambda
  | Display  of disp
  | Literate of string
  | Newline
  | Print of id * id list
  | Curr
  | Curr2
  | Curr3
  | Step
  | StepN of int
  | StepL of dir list
  | Norm
  | MaxSteps of int
  | Quit
[@@deriving sexp_of]

type form =
  | Def  of id * id list * expr
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

let print form = 
  sexp_of_form form |> print_sexp
