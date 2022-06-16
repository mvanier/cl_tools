(* ----------------------------------------------------------------------
 * Types.
 * ---------------------------------------------------------------------- *)

type id = string

type cmd =
  | Curr
  | Norm
  | Step
  | StepC  of atom
  | StepCN of atom * int
  | MaxSteps of int
  | Undo
  | Quit

and prim = S | K | I | B | C | W | M

and atom =
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
  | Cmd  of cmd

type env = (id, expr2) Hashtbl.t

(* ----------------------------------------------------------------------
 * Utility functions on types.
 * ---------------------------------------------------------------------- *)

let rec string_of_cmd = function
  | Curr          -> "Curr"
  | Norm          -> "Norm"
  | Step          -> "Step"
  | StepC  s      ->
    Printf.sprintf "StepC[%s]" (string_of_atom_explicit s)
  | StepCN (s, i) ->
    Printf.sprintf "StepCN[%s, %d]" (string_of_atom_explicit s) i
  | MaxSteps i ->
    Printf.sprintf "MaxSteps[%d]" i
  | Undo          -> "Undo"
  | Quit          -> "Quit"

and string_of_prim = function
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | B -> "B"
  | C -> "C"
  | W -> "W"
  | M -> "M"

and string_of_atom = function
  | Prim p -> string_of_prim p
  | Comb i -> i
  | Var i  -> i

and string_of_atom_explicit = function
  | Prim p -> "PRIM[" ^ string_of_prim p ^ "]"
  | Comb i -> "COMB[" ^ i ^ "]"
  | Var i  -> "VAR[" ^ i ^ "]"

let rec string_of_expr = function
  | Atom a -> string_of_atom a
  | List es ->
      let ss = List.map string_of_expr es in
        "(" ^ (String.concat " " ss) ^ ")"

let rec string_of_expr_explicit = function
  | Atom a -> "ATOM[" ^ string_of_atom_explicit a ^ "]"
  | List es ->
      let ss = List.map string_of_expr_explicit es in
        "LIST[" ^ (String.concat " " ss) ^ "]"

let rec string_of_expr2 = function
  | Atom2 a -> string_of_atom a
  | Pair (x, y) ->
      let sx = string_of_expr2 x in
      let sy = string_of_expr2 y in
        "(" ^ sx ^ " " ^ sy ^ ")"

let string_of_form = function
  | Def (i, e) -> "DEF[" ^ i ^ "][" ^ string_of_expr_explicit e ^ "]"
  | Expr e -> "EXPR[" ^ string_of_expr_explicit e ^ "]"
  | Cmd c -> "CMD[" ^ string_of_cmd c ^ "]"

