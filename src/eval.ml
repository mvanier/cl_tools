open Types

(* ----------------------------------------------------------------------
 * Global state.
 * ---------------------------------------------------------------------- *)

let trace = ref true
let env : env = Hashtbl.create 10

(* ----------------------------------------------------------------------
 * Desugaring.
 * ---------------------------------------------------------------------- *)

(* Desugar an `expr` into an `expr1`. *)

let rec list_to_pairs = function
  | []  -> failwith "empty lists are not allowed"
  | [_] -> failwith "one-element lists are not allowed"
  | [x; y] -> Pair (desugar x, desugar y)
  | (h :: t) -> Pair (desugar h, list_to_pairs t)

and desugar = function
  | Atom a   -> Atom2 a
  | List lst -> reverse (list_to_pairs (List.rev lst))

and reverse = function
  | Atom2 a -> Atom2 a
  | Pair (x, y) -> Pair (reverse y, x)

(* ----------------------------------------------------------------------
 * Environment.
 * ---------------------------------------------------------------------- *)

let add_to_env id expr =
  Hashtbl.replace env id (desugar expr)

let get_env id = Hashtbl.find env id

(* ----------------------------------------------------------------------
 * Evaluator.
 * ---------------------------------------------------------------------- *)

(* Evaluate an `expr2` by reducing to a normal form. *)

let rec evaluate = function
  | Atom2 a -> evaluate_atom a
  | Pair (x, y) ->
    reduce (Pair (evaluate x, evaluate y))

and evaluate_atom = function
  | Prim p -> Atom2 (Prim p)
  | Var i  -> Atom2 (Var i)
  | Comb i -> get_env i

and reduce = function
  | Atom2 a -> Atom2 a
  | Pair (Atom2 (Prim I), x) -> reduce x
  | Pair (Pair (Atom2 (Prim K), x), _) -> reduce x
  | Pair (Pair (Atom2 (Prim W), x), y) ->
    reduce (Pair (Pair (x, y), y))
  | Pair (Pair (Pair (Atom2 (Prim S), x), y), z) ->
    reduce (Pair (Pair (x, z), Pair (y, z)))
  | Pair (Pair (Pair (Atom2 (Prim B), x), y), z) ->
    reduce (Pair (x, Pair (y, z)))
  | Pair (Pair (Pair (Atom2 (Prim C), x), y), z) ->
    reduce (Pair (Pair (x, z), y))
  | Pair (x, y) -> Pair (reduce x, reduce y)

