(* ----------------------------------------------------------------------
 * Types.
 * ---------------------------------------------------------------------- *)

type id = string

type pragma =
  | Trace of bool

type prim = S | K | I | B | C | W

type atom =
  | Prim of prim
  | Comb of id
  | Var of id

type expr =
  | Atom of atom
  | List of expr list

type expr2 =
  | Atom2 of atom
  | Pair of expr2 * expr2

(* Top-level forms *)
type form =
  | Def  of id * expr
  | Expr of expr

type env = (id, expr2) Hashtbl.t

(* ----------------------------------------------------------------------
 * Utility functions on types.
 * ---------------------------------------------------------------------- *)

let string_of_prim = function
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | B -> "B"
  | C -> "C"
  | W -> "W"

let string_of_atom = function
  | Prim p -> string_of_prim p
  | Comb i -> i
  | Var i  -> i

let rec string_of_expr = function
  | Atom a -> string_of_atom a
  | List es ->
      let ss = List.map string_of_expr es in
        "(" ^ (String.concat " " ss) ^ ")"

let rec string_of_expr2 = function
  | Atom2 a -> string_of_atom a
  | Pair (x, y) ->
      let sx = string_of_expr2 x in
      let sy = string_of_expr2 y in
        "(" ^ sx ^ " " ^ sy ^ ")"

let string_of_form = function
  | Def (i, e) -> "DEF[" ^ i ^ "][" ^ string_of_expr e ^ "]"
  | Expr e -> "EXPR[" ^ string_of_expr e ^ "]"
