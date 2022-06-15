open Clclib

open Types
open Eval

(* The REPL, in the form expected by Repl.make_repl. *)
let eval_fn form =
  try
    match eval_form form with
      | None -> ()
      | Some e -> Printf.printf "%s\n%!" (string_of_expr2 e)
  with Failure msg -> Printf.eprintf "%s\n%!" msg

let progname = "clc"

let prompt1 = "   "
let prompt2 = "...  "

let make_repl eval_fn =
  let error_fn () = () in
    begin
      try
        ignore
          (Repl.make_repl prompt1 prompt2 eval_fn error_fn)
      with
        End_of_file ->
          begin
            (* an ugly hack to make exiting look clean *)
            Printf.printf "  \n";
            exit 0
          end
    end

(* Add some derived definitions. *)

let prim p = Atom (Prim p)

let _ = add_to_env "M" (List [prim W; prim I])
let _ = add_to_env "A" (List [prim K; prim I])
let _ = add_to_env "T" (List [prim C; prim I])

(* Entry point. *)
let _ =
  match Sys.argv with
    | [| _ |] -> make_repl eval_fn
    | _ ->
      begin
        Printf.eprintf "usage: %s\n%!" progname;
        exit 1
      end
