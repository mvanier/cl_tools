open Parser_utils
open Types

(* ----------------------------------------------------------------------
 * Global state.
 * ---------------------------------------------------------------------- *)

let env : env = Hashtbl.create 10

let current : expr2 ref = ref (Atom2 (Var ""))

let undos : expr2 Stack.t = Stack.create ()

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

let evaluate_atom = function
  | Prim p -> Atom2 (Prim p)
  | Var i  -> Atom2 (Var i)
  | Comb i -> get_env i

let rec reduce e =
  match e with
    | Atom2 a -> evaluate_atom a
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
    | Pair (x, y) -> 
      let rx = reduce x in
      let ry = reduce y in
        Pair (rx, ry)

let rec eval_expr2 e =
  let re = reduce e in
    if e = re then e else eval_expr2 re

let undo () =
  match Stack.pop_opt undos with
    | None -> Printf.printf "nothing to undo\n%!"
    | Some e ->
        begin
          current := e;
          Printf.printf "%s\n%!" (string_of_expr2 e)
        end

let eval_cmd = function
  | Undo -> undo ()
  | Quit -> exit 0
  | _ -> failwith "TODO"

(* Evaluate a top-level form. *)

let eval_form = function
  | Def (i, e) -> (add_to_env i e; None)
  | Expr e     -> Some (eval_expr2 (desugar e))
  | Cmd p      -> (eval_cmd p; None)

let eval_form_print def =
  match eval_form def with
    | None -> ()
    | Some e -> Printf.printf "%s\n%!" (string_of_expr2 e)

(* ----------------------------------------------------------------------
 * File loading.
 * ---------------------------------------------------------------------- *)

(* Parse and run the contents of an input buffer. *)
let eval_from_input input =
  (* Parse the file to top-level forms. *)
  let res = Parser.parse_many input in
    match res with
      | Error (l, msg) -> Error (l, msg)
      | Incomplete -> Incomplete
      | Ok es ->
          begin
            (* Evaluate the contents of the file. *)
            List.iter eval_form_print es;
            Ok ()
          end

let load_file filename =
  (* Read in and parse the file. *)
  let channel =
    try open_in filename
    with Sys_error _ ->
      begin
        Printf.eprintf "file not found: %s\n%!" filename;
        exit 1
      end
  in
  let lexbuf = Lexing.from_channel channel in
  let input  = make_input filename lexbuf in
    match eval_from_input input with
      | Error (l, msg) ->
        begin
          close_in channel;
          Error (l, msg)
        end
      | Incomplete ->
        begin
          close_in channel;
          Incomplete
        end
      | Ok () ->
        begin
          close_in channel;
          Ok ()
        end
