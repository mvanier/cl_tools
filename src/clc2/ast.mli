(* Abstract syntax tree. *)

open Utils

type dir = L | R
[@@deriving sexp_of]

type disp =
  | Normal    (* Use the standard display conventions. *)
  | Raw       (* Show all applications explicitly. *)
[@@deriving sexp_of]

type converter =
  | SKI
  | SKIBC
  | BCKW
[@@deriving sexp_of]

type lambda =
  | LVar of id
  | LApp of lambda * lambda
  | LLam of id * lambda
[@@deriving sexp_of]

type cmd =
  | Convert of converter * lambda
      (* convert lambda expression to combinators *)
  | Display of disp        (* set display mode *)
  | Literate of string     (* print a literate comment *)
  | Newline                (* print a newline *)
  | Print of id * id list  (* print a definition *)
  | Curr                   (* print current expression *)
  | Curr2                  (* print current raw expression *)
  | Curr3                  (* print current raw expression, annotated *)
  | Step                   (* evaluate one step *)
  | StepN of int           (* evaluate N steps *)
  | StepL of dir list      (* evaluate one step of a subexpression *)
  | Norm                   (* normalize the current expression *)
  | MaxSteps of int        (* set the maximum number of reduction steps *)
  | Quit                   (* exit the interpreter *)
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
