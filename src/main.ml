open Clclib

open Loc
open Lexer_utils
open Parser_utils
open Types
open Eval

(* The REPL, in the form expected by Repl.make_repl. *)
let eval_fn form =
  try
    eval_form form
  with Failure msg -> Printf.eprintf "%s\n%!" msg

let print_error loc msg =
  Printf.eprintf "Error: %s (%s)\n%!" msg (string_of_loc_short loc)

let handle_fatal_parse_errors result =
  match result with
    | Incomplete ->
      begin
        Printf.eprintf
          "Incomplete form at end of input; exiting.\n%!";
        exit 1
      end
    | Error (l, msg) ->
      begin
        print_error l msg;
        exit 1
      end
    | Ok x -> x

let handle_fatal_exceptions thunk =
  try
    thunk ()
  with
    | Lexer_error (l, err) ->
      begin
        print_error l (string_of_lex_error err);
        exit 1
      end

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
    | [| _; filename |] ->
      ignore
        (handle_fatal_exceptions
          (fun () ->
             handle_fatal_parse_errors
               (load_file filename)))
    | _ ->
      begin
        Printf.eprintf "usage: %s\n%!" progname;
        exit 1
      end

