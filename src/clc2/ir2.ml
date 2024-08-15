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

type comb =
  {
    arity : int;
    body : dexpr;
  }
[@@deriving sexp_of]

type form =
  | Def  of id * comb
  | Expr of expr
  | Cmd  of cmd
[@@deriving sexp_of]

let print_expr expr = 
  sexp_of_expr expr |> print_sexp

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

let rec convert_def_expr vars e =
  match e with
    | I.Var id ->
      begin
        match List.find_index (fun v -> id = v) vars with
          | None -> compile_err "invalid combinator"
          | Some i -> DVar i
      end
    | I.Const c -> DConst c
    | I.App (e1, e2) ->
        DApp (convert_def_expr vars e1, convert_def_expr vars e2)

let convert_def vars e =
  let len = List.length vars in
  let de = convert_def_expr vars e in
    { arity = len; body = de }

let convert form =
  match form with
    | I.Def (id, vars, e) -> Def (id, convert_def vars e)
    | I.Expr e -> Expr (convert_expr e)
    | I.Cmd c -> Cmd c

let spprint_expr expr =
  let rec left_flatten e =
    match e with
      | App (e1, e2) -> left_flatten e1 @ [e2]
      | _ -> [e]
  in
  let strip_parens s =
    let len = String.length s in
    if s.[0] = '(' && s.[len - 1] = ')' then
      String.sub s 1 (len - 2)
    else
      s
  in
  let rec show e =
    match e with
      | Var id
      | Const id -> id
      | App _ ->
          "(" ^ String.concat " " (List.map show (left_flatten e)) ^ ")"
  in
    strip_parens (show expr)

let pprint_expr ?(prefix = "--> ") expr =
  Printf.printf "%s%s" prefix (spprint_expr expr)

let pprint_expr2 expr =
  let rec show e =
    match e with
      | Var id
      | Const id -> id
      | App (e1, e2) -> "(" ^ show e1 ^ " " ^ show e2 ^ ")"
  in
    Printf.printf "%s\n%!" (show expr)

