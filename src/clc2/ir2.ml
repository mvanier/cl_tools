open Sexplib.Conv
open Pprint
open Utils

module I = Ir

type cmd = Ast.cmd
[@@deriving sexp_of]

type expr =
  | Var   of id
  | Const of id
  | App   of expr * expr
[@@deriving sexp_of]

type dexpr =
  | DVar   of int
  | DConst of id
  | DApp   of dexpr * dexpr
[@@deriving sexp_of]

type form =
  | Def  of int * dexpr
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

let print form = 
  sexp_of_form form |> print_sexp

(*
 * Conversion from IR.
 *)

let convert form =
  failwith "TODO"

