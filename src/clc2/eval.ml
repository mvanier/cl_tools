open Utils
open Ir2

let _debug msg = Printf.printf "%s\n%!" msg

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

let get_env id = Hashtbl.find_opt env id

(* ----------------------------------------------------------------------
 * Utilities.
 * ---------------------------------------------------------------------- *)

(* Partition a list of elements into a list of lists
   based on a number that can be computed from each element.
   Sort the resulting list of lists by that number. *)
let partition (lst : 'a list) (f : 'a -> int) : 'a list list =
  (* Convert the list elements to a list of (number, element) pairs. *)
  let lst' = List.map (fun e -> (f e, e)) lst in
  (* Collect all the numbers. *)
  let nums = List.map fst lst' in
  (* Sort the number list. *)
  let nums' = List.sort_uniq compare nums in
  (* Collect the list of elements for each number. *)
    List.map
      (fun n ->
         List.filter_map
           (fun (n', e) -> if n' = n then Some e else None)
             lst')
      nums'

(* Flatten an expression from the leftmost side. *)
let rec left_flatten e =
  match e with
    | App (e1, e2) -> left_flatten e1 @ [e2]
    | _ -> [e]

(* Convert a dexpr to an expr.
   This only works if the expression has no variables. *)
let rec expr_of_dexpr d =
  match d with
    | DVar _ -> runtime_err "expr_of_dexpr: no variables allowed"
    | DConst id -> Const id
    | DApp (d1, d2) -> App (expr_of_dexpr d1, expr_of_dexpr d2)

(* Reduce the outermost redex of an expression.
   Return `None` if the expression can't be reduced,
   `Some <new_expr>` if it can. *)
let rec reduce e =
  (* Apply a combinator to its arguments, generating a new expression. *)
  let rec apply body args =
    match body with
      | DVar i -> List.nth args i
      | DConst id -> Const id
      | DApp (d1, d2) -> App (apply d1 args, apply d2 args)
  in
  (* ALGORITHM:
     - If the expression is a variable, return None.
     - If the expression is a constant:
       - reduce it if it has arity 0;
       - otherwise, return None.
     - Otherwise, it's an application. Left-flatten the expression.
       - If its length is < 2, return None.
       - If the first element is a constant
         whose arity is the correct length, reduce that.
       - Otherwise try reducing the left expression,
         then (if that fails) the right.
   *)
  let aux e =
    match e with
      | Var _ -> None
      | Const id ->
        begin
          match get_env id with
            | None -> None
            | Some e' -> 
                if e'.arity = 0 then
                  Some (expr_of_dexpr e'.body)
                else
                  None
        end
      | App _ ->
          let le = left_flatten e in
          let len = List.length le in
            begin
              if len < 2 then
                None
              else
                match le with
                  | Const c :: args ->
                    begin
                      match get_env c with
                        | None -> None
                        | Some def -> 
                            if def.arity = len - 1 then
                              Some (apply def.body args)
                            else
                              None
                    end
                      
                  | _ -> None
            end
  in
    match aux e with
      | None ->  (* Couldn't reduce full expression. *)
        begin
          match e with
            | App (e1, e2) ->
              begin
                (* Try reducing left subexpression. *)
                match reduce e1 with
                  | None ->
                    begin
                      (* Try reducing right subexpression. *)
                      match reduce e2 with
                        | None -> None
                        | Some e' -> Some (App (e1, e'))
                    end
                  | Some e' -> Some (App (e', e2))
              end
            | _ -> None
        end
      | Some e' -> Some e'

(* Take one step in the current expression.
   Print the result if any. *)
let step1 e =
  match reduce e with
    | None -> None
    | Some e' ->
      begin
        pprint_expr e';
        Some e'
      end

(* Take one step from the current expression; if reduced,
   print the result and assign the resulting expr to current expr *)
let step () =
  match !current with
    | None -> runtime_err "no current expression"
    | Some e ->
      begin
        match step1 e with
          | None -> ()
          | Some e' -> current := Some e'
      end

(* Reduce to a normal form, if any. *)
let norm () =
  let rec iter i e =
    if i >= !max_reductions then
      runtime_err "too many reductions - infinite loop?"
    else
      match step1 e with
        | None -> ()  (* done reducing *)
        | Some e' -> iter (i + 1) e'
  in
    match !current with
      | None -> runtime_err "no current expression"
      | Some e ->
        begin
          pprint_expr ~prefix:"" e;
          iter 0 e
        end

let curr () =
  match !current with
    | None -> runtime_err "no current expression"
    | Some e -> pprint_expr ~prefix:"" e

let curr2 () =
  match !current with
    | None -> runtime_err "no current expression"
    | Some e -> pprint_expr2 e

let curr3 () =
  let analyze e : (int * int * int) list =
    let collect = Dynarray.create () in
    let indent = ref 0 in
    let rec iter e depth index =
      match e with
        | Var id ->
            indent := !indent + String.length id
        | Const id ->
          begin
            Dynarray.add_last collect (!indent, depth, index);
            indent := !indent + String.length id
          end
        | App (e1, e2) ->
          begin
            Dynarray.add_last collect (!indent, depth, index);
            indent := !indent + 1; (* left parenthesis *)
            iter e1 (depth + 1) 0;
            indent := !indent + 1; (* space between e1 and e2 *)
            iter e2 (depth + 1) 1;
            indent := !indent + 1  (* right parenthesis *)
          end
    in
      let _ = iter e 0 0 in
      let lst = Dynarray.to_list collect in
        (* Get rid of the depth 0 element. *)
        List.filter (fun (n, _, _) -> n > 0) lst
  in
  let display (lst : (int * int * int) list list) : unit =
    let display_line (lst : (int * int) list) : unit =
      (* Find the largest indent in the line.
         Make a string buffer that is that many characters long,
         containing only blank characters.
         Set the 0s and 1s at the appropriate locations.
         Convert to a string and print it. *)
      let max_indent =
        List.fold_left (fun m (i, _) -> max m i) 0 lst
      in
      let buffer = Bytes.make (max_indent + 1) ' ' in
        begin
          List.iter
            (fun (i, n) ->
               Bytes.set buffer i
                 (match n with
                    | 0 -> '0'
                    | 1 -> '1'
                    | -1 -> '|'
                    | _ -> failwith "display_line: invalid index"))
            lst;
          Printf.printf "%s\n%!" (Bytes.to_string buffer)
        end
    in
    let display_lines (lst : (int * int * int) list) : unit =
      lst
        |> List.map (fun (indent, _, index) -> (indent, index))
        |> display_line
    in
      List.iter display_lines lst
  in
    match !current with
      | None -> runtime_err "no current expression"
      | Some e ->
          let data = analyze e in
          let data2 = partition data (fun (_, i, _) -> i) in
            begin
              pprint_expr2 e;
              (*
              List.iter
                (fun (indent, depth, index) ->
                   Printf.printf "%d, %d, %d\n" indent depth index)
                data;
              *)
              (*
              List.iter
                (fun lst ->
                   List.iter
                     (fun (indent, depth, index) ->
                        Printf.printf "%d, %d, %d\n" depth indent index)
                     lst) 
                data2;
              *)
              display data2
            end

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
      | Literate s ->
          print_endline s
      | Newline ->
          print_endline ""
      | Curr ->
          curr ()
      | Curr2 ->
          curr2 ()
      | Curr3 ->
          curr3 ()
      | Step ->
          step ()
      | StepN n ->
        begin
          for _ = 1 to n do
            step ()
          done
        end
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
