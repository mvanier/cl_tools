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

let debug e =
  if !trace then
    Printf.printf "-> %s\n%!" (string_of_expr2 e)

let evaluate_atom = function
  | Prim p -> Atom2 (Prim p)
  | Var i  -> Atom2 (Var i)
  | Comb i -> get_env i

let rec eval_expr2 e =
  debug e;
  match e with
    | Atom2 a -> evaluate_atom a
    | Pair (Atom2 (Prim I), x) -> eval_expr2 x
    | Pair (Pair (Atom2 (Prim K), x), _) -> eval_expr2 x
    | Pair (Pair (Atom2 (Prim W), x), y) ->
      eval_expr2 (Pair (Pair (x, y), y))
    | Pair (Pair (Pair (Atom2 (Prim S), x), y), z) ->
      eval_expr2 (Pair (Pair (x, z), Pair (y, z)))
    | Pair (Pair (Pair (Atom2 (Prim B), x), y), z) ->
      eval_expr2 (Pair (x, Pair (y, z)))
    | Pair (Pair (Pair (Atom2 (Prim C), x), y), z) ->
      eval_expr2 (Pair (Pair (x, z), y))
    | Pair (x, y) -> 
      let rx = eval_expr2 x in
      let ry = eval_expr2 y in
        Pair (rx, ry)

let eval_pragma = function
  | "trace_off" -> trace := false
  | "trace_on"  -> trace := true
  | s -> failwith ("unknown pragma: " ^ s)

(* Evaluate a top-level form. *)

let eval_form = function
  | Def (i, e) -> (add_to_env i e; None)
  | Expr e     -> Some (eval_expr2 (desugar e))
  | Pragma p   -> (eval_pragma p; None)

