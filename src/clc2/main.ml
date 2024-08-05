(* The interpreter: entry point. *)

open Clc2lib

let progname = "clc2"

let repl_test () =
  let lexbuf = Lexing.from_channel stdin in
  let f = use_lexbuf_repl "<repl>" lexbuf in
  let prompt = ">> " in
  let rec iter () =
    begin
      Printf.printf "%s%!" prompt;
      try
        match f env with
          | None -> 
            begin
              Printf.printf "  \n%!";  (* an ugly hack to make exiting look clean *)
              exit 0
            end
          | Some (env', v) ->
            begin
              (* TODO: Print the form. *)
              iter env'
            end
      with
        | Lexer.Lexer_error (l, err) ->
          begin
            Printf.printf "Lexer error: %s\n%!"
              (Lexer.string_of_lex_error err);
            iter env
          end
    end
  in
    iter env

let _ = 
  match Sys.argv with
    | [| _ |] ->
      begin
        try 
          repl_test ()
        with 
          End_of_file ->
            begin
              Printf.printf "  \n%!";  (* an ugly hack to make exiting look clean *)
              exit 0
            end
      end
    | _ -> 
      begin
        Printf.eprintf "usage: %s\n%!" progname;
        exit 1;
      end

