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
  | Prim _ -> None
  | Var _  -> None
  | Comb i -> Some (get_env i)

(* Reduce the outermost expression.
 * Return None if the expression can't be reduced,
 * Some <new_expr> if it can. *)
let reduce e =
  match e with
    | Atom2 a -> evaluate_atom a
    | Pair (Atom2 (Prim I), x) -> Some x
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

(* Reduce the topmost reducible expression
 * Return None if the expression can't be reduced,
 * Some <new_expr> if it can. *)
let rec step verbose e =
  match reduce e with
    | None ->
      begin
        match e with
          | Pair (x, y) ->
            begin
              match step verbose x with
                | None -> 
                  begin
                    match step verbose y with
                      | None -> None
                      | Some re -> Some (Pair (x, re))
                  end
                | Some re -> Some (Pair (re, y))
            end
          | _ -> None
      end
    | Some re -> 
      begin
        (* Push the previous expression onto the stack. *)
        Stack.push e undos;
        if verbose then
          Printf.printf "-> %s\n%!" (string_of_expr2 re);
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
      match step true e with
        | None -> if i = 0 then None else Some e
        | Some re -> iter (i + 1) re
  in
    begin
      Printf.printf "== %s\n%!" (string_of_expr2 e);
      iter 0 e
    end

let undo () =
  match Stack.pop_opt undos with
    | None -> Printf.printf "nothing to undo\n%!"
    | Some e ->
        begin
          current := Some e;
          Printf.printf "%s\n%!" (string_of_expr2 e)
        end

let eval_cmd = function
  | Undo -> undo ()
  | Quit -> exit 0
  | Curr ->
    begin
      match !current with
        | None -> Printf.printf "no current expression\n%!"
        | Some e -> Printf.printf "%s\n%!" (string_of_expr2 e)
    end
  | Norm ->
    begin
      match !current with
        | None -> Printf.printf "no current expression\n%!"
        | Some e ->
          begin
            match norm e with
              | None -> Printf.printf "%s\n%!" (string_of_expr2 e)
              | Some re ->
                begin
                  Printf.printf "%s\n%!" (string_of_expr2 re)
                end
          end
    end
  | Step ->
    begin
      match !current with
        | None -> Printf.printf "no current expression\n%!"
        | Some e ->
          begin
            match step false e with
              | None -> Printf.printf "%s\n%!" (string_of_expr2 e)
              | Some re ->
                begin
                  Printf.printf "%s\n%!" (string_of_expr2 re)
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
