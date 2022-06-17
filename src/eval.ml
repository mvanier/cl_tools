open Parser_utils
open Types

(* ----------------------------------------------------------------------
 * Global state.
 * ---------------------------------------------------------------------- *)

let env : env = Hashtbl.create 10

let current : expr2 option ref = ref None

let undos : expr2 Stack.t = Stack.create ()

let max_reductions = ref 25 

let literate_mode = ref false

let prefix () = if !literate_mode then "  " else ""

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
            Printf.printf "%s-> %s\n%!" (prefix ()) (display re);
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
      Printf.printf "%s== %s\n%!" (prefix ()) (display e);
      iter 0 e
    end

let undo () =
  match Stack.pop_opt undos with
    | None -> Printf.printf "nothing to undo\n%!"
    | Some e ->
        begin
          current := Some e;
          Printf.printf "%s%s\n%!" (prefix ()) (display e)
        end

let step1 () =
  match !current with
    | None -> Printf.printf "no current expression\n%!"
    | Some e ->
      begin
        match step false reduce e with
          | None -> Printf.printf "%s-> %s\n%!" (prefix ()) (display e)
          | Some re ->
            begin
              Printf.printf "%s-> %s\n%!" (prefix ()) (display re)
            end
      end

let eval_cmd = function
  | Undo -> undo ()

  | Quit -> exit 0

  | MaxSteps i -> max_reductions := i

  | Curr ->
    begin
      match !current with
        | None -> Printf.printf "%sno current expression\n%!" (prefix ())
        | Some e -> Printf.printf "%s%s\n%!" (prefix ()) (display e)
    end

  | Norm ->
    begin
      match !current with
        | None -> Printf.printf "%sno current expression\n%!" (prefix ())
        | Some e ->
          begin
            match norm e with
              | None -> Printf.printf "%s%s\n%!" (prefix ()) (display e)
              | Some re ->
                begin
                  Printf.printf "%s%s\n%!" (prefix ()) (display re)
                end
          end
    end

  | Step -> step1 ()

  | StepN n -> for _ = 1 to n do step1 () done

  | StepC a ->
    begin
      match !current with
        | None -> Printf.printf "%sno current expression\n%!" (prefix ())
        | Some e ->
          begin
            match step false (reduce_if a) e with
              | None -> Printf.printf "%s-> %s\n%!" (prefix ()) (display e)
              | Some re ->
                begin
                  Printf.printf "%s-> %s\n%!" (prefix ()) (display re)
                end
          end
    end

  | _ -> failwith "TODO"

(* Evaluate a top-level form. *)

let eval_form = function
  | Def (i, e) -> 
    begin
      if !literate_mode then
        Printf.printf "%sdef %s %s\n%!"
          (prefix ()) i (string_of_expr e);
      add_to_env i e
    end

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

(* File extension for literate files (.lclc). *)
let literate_ext = Str.regexp {|.*\.lclc$|}

(* Return `true` if the filename ends in
 * the literate extension. *)
let is_literate filename =
  try
    begin
      ignore (Str.search_forward literate_ext filename 0);
      true
    end
  with Not_found ->
    false

(* Process a line.  The trailing newline is assumed to have been removed. *)
let process_literate_line line =
  let prepend = ";;|" in
  let len = String.length line in
    if len = 0 then
      prepend   (* empty literate line *)
    else if line.[0] = '>' then
      (* Remove the next character if it's a blank. *)
      (if line.[1] = ' ' then
         String.sub line 2 (len - 2)
       else
         String.sub line 1 (len - 1))
    else
      prepend ^ " " ^ line

(* Convert a literate file to a non-literate file,
 * and return the contents as a string. *)
let process_literate_file in_channel =
  (* Read a file, split into lines.
   * Convert each line.  Combine the converted lines. *)
  let rec iter lines =
    try
      let next = input_line in_channel in
      let next' = process_literate_line next in
        iter (next' :: lines)
    with End_of_file ->
      List.rev lines
  in
    String.concat "\n" (iter []) ^ "\n"

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
  let lexbuf =
    if is_literate filename then
      begin
        literate_mode := true;
        Lexing.from_string (process_literate_file channel)
      end
    else
      Lexing.from_channel channel
  in
  let input = make_input filename lexbuf in
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
