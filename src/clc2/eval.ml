open Utils
open Ir2

(* ----------------------------------------------------------------------
 * Global state.
 * ---------------------------------------------------------------------- *)

let env = Hashtbl.create 100

let current = ref None

let max_reductions = ref 25 

(* ----------------------------------------------------------------------
 * Environment.
 * ---------------------------------------------------------------------- *)

let add_to_env id def =
  Hashtbl.replace env id def

let get_env id = Hashtbl.find env id

(* ----------------------------------------------------------------------
 * Utilities.
 * ---------------------------------------------------------------------- *)

(* Display an expression. *)
let show_expr e =
  print_expr e

(* Reduce the outermost redex of an expression.
   Return `None` if the expression can't be reduced,
   `Some <new_expr>` if it can. *)
let reduce e =
  failwith "TODO"

(* Take one step in the current expression.
   Print the result if any. *)
let step e =
  match reduce e with
    | None -> None
    | Some e' ->
      begin
        show_expr e';
        Some e'
      end

(* Reduce to a normal form, if any. *)
let norm () =
  let rec iter i e =
    if i >= !max_reductions then
      runtime_err "too many reductions - infinite loop?"
    else if i = 0 then
      ()
    else
      match step e with
        | None -> ()  (* done reducing *)
        | Some e' -> iter (i + 1) e'
  in
    match !current with
      | None -> runtime_err "no current expression"
      | Some e -> iter 0 e

let set_max_steps i =
  if i > 0 then
    max_reductions := i
  else
    runtime_err "maximum number of reductions must be > 0"

let quit () =
  begin
    Printf.printf "  \n%!";  (* an ugly hack to make exiting look clean *)
    exit 0
  end

(* ----------------------------------------------------------------------
 * Evaluator.
 * ---------------------------------------------------------------------- *)

let eval_def id def =
  add_to_env id def

let eval_expr e =
  current := Some e

let eval_cmd c =
  let open Ast in
    match c with
      | Step ->
          failwith "TODO"
          (* step and assign resulting expr to current expr *)
          (* step () *)
      | Norm ->
          norm ()
      | MaxSteps i ->
          set_max_steps i
      | Quit ->
          quit ()

let eval_form form =
  match form with
    | Def (id, def) -> eval_def id def
    | Expr e -> eval_expr e
    | Cmd c -> eval_cmd c
