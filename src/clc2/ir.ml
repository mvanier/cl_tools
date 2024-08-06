open Sexplib.Conv
open Pprint

module A = Ast

exception Parse_error of string

type id = string
[@@deriving sexp_of]

type cmd = A.cmd
[@@deriving sexp_of]

type expr =
  | Var   of id
  | Const of id
  | App   of expr * expr
[@@deriving sexp_of]

type form =
  | Def  of id * id list * expr
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

let print form = 
  sexp_of_form form |> print_sexp

(*
 * Conversion from AST.
 *)

let parse_err msg = raise (Parse_error msg)

let rec fold es =
  match es with
    | []
    | [_] -> parse_err "fold: too few expressions"
    | [e1; e2] -> App (e1, e2)
    | e1 :: e2 :: es' -> fold (App (e1, e2) :: es')

let rec convert_expr expr =
  match expr with
    | A.Var id -> Var id
    | A.Const id -> Const id
    | A.List es ->
        let es' = List.map convert_expr es in
          fold es'

let convert form =
  match form with
    | A.Def (id, ids, e) -> Def (id, ids, convert_expr e)
    | A.Expr e -> Expr (convert_expr e)
    | A.Cmd c -> Cmd c

