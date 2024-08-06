(* The interpreter: entry point. *)

open Clc2lib

let progname = "clc2"

let repl_test () =
  let lexbuf = Lexing.from_channel stdin in
  let lex = Lexer.lex "<repl>" in
  let prompt = ">> " in
  let rec iter () =
    begin
      Printf.printf "%s%!" prompt;
      try
        match Parser.repl lex lexbuf with
          | None -> iter ()
          | Some ast ->
            begin
              Printf.printf "AST:\n\n";
              Ast.print ast;
              let ir = Ir.convert ast in
                begin
                  Printf.printf "\nIR:\n\n";
                  Ir.print ir;
                  Printf.printf "\n%!";
                  iter ()
                end
            end
      with
        | Lexer.Lexer_error err ->
          begin
            Lexing.flush_input lexbuf;
            Printf.printf "Lexer error: %s\n%!"
              (Lexer.string_of_lex_error err);
            iter ()
          end
        | Parser.Error _ ->
            let pos = Lexing.lexeme_start_p lexbuf in
            let pos = { pos with pos_fname = "<repl>" } in
            let loc =
              Printf.sprintf "file: %s, line: %d, char: %d"
                pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
            in
              begin
                Lexing.flush_input lexbuf;
                Printf.printf "Parser error: %s\n%!" loc;
                iter ()
              end
        | Ir.Parse_error msg ->
            begin
              Printf.printf "Parse error: %s\n%!" msg;
              iter ()
            end
    end
  in
    iter ()

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

