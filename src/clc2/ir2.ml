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

let rec convert_expr e =
  match e with
    | I.Var id -> Var id
    | I.Const id -> Const id
    | I.App (e1, e2) -> App (convert_expr e1, convert_expr e2)

let convert_def id vars e =
  failwith "TODO"

let convert form =
  match form with
    | I.Def (id, vars, e) -> convert_def id vars e
    | I.Expr e -> Expr (convert_expr e)
    | I.Cmd c -> Cmd c

