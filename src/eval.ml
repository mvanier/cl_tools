open Parser_utils
open Types

(* ----------------------------------------------------------------------
 * Global state.
 * ---------------------------------------------------------------------- *)

let env : env = Hashtbl.create 10

let current : expr2 option ref = ref None

let undos : expr2 Stack.t = Stack.create ()

let max_reductions = ref 25 

(* ----------------------------------------------------------------------
 * Desugaring.
 * ---------------------------------------------------------------------- *)

(* Desugar an `expr` into an `expr2`. *)

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
 * Resugaring.
 * ---------------------------------------------------------------------- *)

(* Resugar an `expr2` into an `expr`.
 * This is for display purposes. *)

let rec expr_of_expr2 = function
  | Atom2 a -> Atom a
  | Pair (Atom2 a, e2) -> List [Atom a; expr_of_expr2 e2]
  | Pair (Pair (x, y), e2) ->
    let e1a = expr_of_expr2 x in
    let e1b = expr_of_expr2 y in
    let e2' = expr_of_expr2 e2 in
      match e1a with
        | List lst -> List (lst @ [e1b; e2'])
        | _ -> List [e1a; e1b; e2']

(* Convert an expr2 to an expr and then to a string. *)
let string_of_expr2_expr e =
  string_of_expr (expr_of_expr2 e)

(* Default display of expressions. *)
let display e = string_of_expr2_expr e

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
  | Prim _ -> None
  | Var _  -> None
  | Comb c -> Some (get_env c)

(* Reduce the outermost expression.
 * Return None if the expression can't be reduced,
 * Some <new_expr> if it can. *)
let reduce e =
  match e with
    | Atom2 a -> evaluate_atom a
    | Pair (Atom2 (Prim I), x) -> Some x
    | Pair (Atom2 (Prim M), x) -> Some (Pair (x, x))
    | Pair (Pair (Atom2 (Prim K), x), _) -> Some x
    | Pair (Pair (Atom2 (Prim W), x), y) ->
      Some (Pair (Pair (x, y), y))
    | Pair (Pair (Pair (Atom2 (Prim S), x), y), z) ->
      Some (Pair (Pair (x, z), Pair (y, z)))
    | Pair (Pair (Pair (Atom2 (Prim B), x), y), z) ->
      Some (Pair (x, Pair (y, z)))
    | Pair (Pair (Pair (Atom2 (Prim C), x), y), z) ->
      Some (Pair (Pair (x, z), y))
    | _ -> None

(* Reduce the outermost expression if it starts with
 * a particular combinator. *)
let reduce_if a e =
  match (a, e) with
    | (Var _, _) ->
      failwith "reduce_if: can't reduce vars"

    | (Comb c, Atom2 (Comb c')) when c = c' ->
      Some (get_env c)

    | (Comb c, Pair (Atom2 (Comb c'), x)) when c = c' ->
      Some (Pair (get_env c, x))

    | (Prim I, Pair (Atom2 (Prim I), x)) -> Some x

    | (Prim M, Pair (Atom2 (Prim M), x)) ->
      Some (Pair (x, x))

    | (Prim K, Pair (Pair (Atom2 (Prim K), x), _)) ->
      Some x

    | (Prim W, Pair (Pair (Atom2 (Prim W), x), y)) ->
      Some (Pair (Pair (x, y), y))

    | (Prim S, Pair (Pair (Pair (Atom2 (Prim S), x), y), z)) ->
      Some (Pair (Pair (x, z), Pair (y, z)))

    | (Prim B, Pair (Pair (Pair (Atom2 (Prim B), x), y), z)) ->
      Some (Pair (x, Pair (y, z)))

    | (Prim C, Pair (Pair (Pair (Atom2 (Prim C), x), y), z)) ->
      Some (Pair (Pair (x, z), y))

    | _ -> None

(* Reduce the topmost reducible expression
 * Return None if the expression can't be reduced,
 * Some <new_expr> if it can.
 * This is parameterized around a reduction function.*)
let step verbose reducef e =
  let rec iter e =
    match reducef e with
      | None ->
        begin
          match e with
            | Pair (x, y) ->
              begin
                match iter x with
                  | None -> 
                    begin
                      match iter y with
                        | None -> None
                        | Some re -> Some (Pair (x, re))
                    end
                  | Some re -> Some (Pair (re, y))
              end
            | _ -> None
        end
      | Some re -> Some re
  in
    match iter e with
      | None -> None
      | Some re ->
        begin
          (* Push the previous expression onto the stack. *)
          Stack.push e undos;
          if verbose then
            Printf.printf "-> %s\n%!" (display re);
          (* Make the reduced expression the current expression. *)
          current := Some re;
          Some re
        end

(* Reduce to a normal form, if any. *)
let norm e =
  let rec iter i e =
    if i >= !max_reductions then
      failwith "ERROR: infinite reduction loop detected"
    else
      match step true reduce e with
        | None -> if i = 0 then None else Some e
        | Some re -> iter (i + 1) re
  in
    begin
      Printf.printf "== %s\n%!" (display e);
      iter 0 e
    end

let undo () =
  match Stack.pop_opt undos with
    | None -> Printf.printf "nothing to undo\n%!"
    | Some e ->
        begin
          current := Some e;
          Printf.printf "%s\n%!" (display e)
        end

let step1 () =
  match !current with
    | None -> Printf.printf "no current expression\n%!"
    | Some e ->
      begin
        match step false reduce e with
          | None -> Printf.printf "-> %s\n%!" (display e)
          | Some re ->
            begin
              Printf.printf "-> %s\n%!" (display re)
            end
      end

let eval_cmd = function
  | Undo -> undo ()

  | Quit -> exit 0

  | MaxSteps i -> max_reductions := i

  | Curr ->
    begin
      match !current with
        | None -> Printf.printf "no current expression\n%!"
        | Some e -> Printf.printf "%s\n%!" (display e)
    end

  | Norm ->
    begin
      match !current with
        | None -> Printf.printf "no current expression\n%!"
        | Some e ->
          begin
            match norm e with
              | None -> Printf.printf "%s\n%!" (display e)
              | Some re ->
                begin
                  Printf.printf "%s\n%!" (display re)
                end
          end
    end

  | Step -> step1 ()

  | StepN n -> for _ = 1 to n do step1 () done

  | StepC a ->
    begin
      match !current with
        | None -> Printf.printf "no current expression\n%!"
        | Some e ->
          begin
            match step false (reduce_if a) e with
              | None -> Printf.printf "-> %s\n%!" (display e)
              | Some re ->
                begin
                  Printf.printf "-> %s\n%!" (display re)
                end
          end
    end

  | _ -> failwith "TODO"

(* Evaluate a top-level form. *)

let eval_form = function
  | Def (i, e) -> add_to_env i e

  | Expr e -> 
    let de = desugar e in
      begin
        Stack.clear undos;
        current := Some de;
      end

  | Cmd p -> eval_cmd p

  | Txt s -> Printf.printf "%s\n%!" s

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
            List.iter eval_form es;
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
