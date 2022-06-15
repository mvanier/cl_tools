(** Types. *)

(** Identifiers. *)
type id = string

type pragma = Trace of bool

(** Primitive combinators. *)
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

type form =
  | Def of id * expr
  | Expr of expr

type env = (id, expr2) Hashtbl.t

val string_of_prim : prim -> string

val string_of_atom : atom -> string

val string_of_atom_explicit : atom -> string

val string_of_expr : expr -> string

val string_of_expr_explicit : expr -> string

val string_of_expr2 : expr2 -> string

val string_of_form : form -> string
