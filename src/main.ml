open Clclib

open Types
open Lexer_utils
open Parser_utils
open Eval

(* The REPL, in the form expected by Repl.make_repl. *)
let eval_fn def =
  try
    eval_def def
  with Failure msg -> Printf.eprintf "%s\n%!" msg

let progname = "clc"

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
    Lexer_error (l, err) ->
      begin
        print_error l (string_of_lex_error err);
        exit 1
      end

let prompt1 = "   "
let prompt2 = "...  "

let make_repl vm eval_fn =
  let error_fn () = () in
    begin
      try
        ignore
          (Repl.make_repl prompt1 prompt2 eval_fn error_fn vm)
      with
        End_of_file ->
          begin
            (* an ugly hack to make exiting look clean *)
            Printf.printf "  \n";
            exit 0
          end
    end

(* Add some derived definitions. *)
let _ = add_to_env "M" (List [w; i])
let _ = add_to_env "A" (List [k; i])
let _ = add_to_env "T" (List [c; i])

(* Entry point. *)
let _ =
  match Sys.argv with
    | [| _ |] -> make_repl vm eval_fn
    | _ ->
      begin
        Printf.eprintf "usage: %s\n%!" progname;
        exit 1
      end
