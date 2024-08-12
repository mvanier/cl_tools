(* The interpreter: entry point. *)

open Clc2lib
open Utils

let progname = "clc2"

let handle_lex_error lexbuf err =
  begin
    Lexing.flush_input lexbuf;
    Printf.printf "Lexer error: %s\n%!"
      (Lexer.string_of_lex_error err);
  end

let handle_parser_error lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let pos = { pos with pos_fname = "<repl>" } in
  let loc =
    Printf.sprintf "file: %s, line: %d, char: %d"
      pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
    begin
      Lexing.flush_input lexbuf;
      Printf.printf "Parser error: %s\n%!" loc;
    end

let repl_test () =
  let lexbuf = Lexing.from_channel stdin in
  let lex = Lexer.lex "<repl>" in
  let prompt = ">> " in
  let rec iter () =
    begin
      Printf.printf "%s%!" prompt;
      begin
        try
          match Parser.repl lex lexbuf with
            | None -> iter ()
            | Some ast ->
                let ir = Ir.convert ast in
                let ir2 = Ir2.convert ir in
                  begin
                    Printf.printf "AST:\n\n";
                    Ast.print ast;
                    Printf.printf "\nIR:\n\n";
                    Ir.print ir;
                    Printf.printf "\nIR2:\n\n";
                    Ir2.print ir2;
                    Printf.printf "\n%!";
                  end
        with
          | Lexer.Lexer_error err ->
              handle_lex_error lexbuf err
          | Parser.Error _ ->
              handle_parser_error lexbuf
          | Parse_error msg ->
              Printf.printf "Parse error: %s\n%!" msg
          | Compile_error msg ->
              Printf.printf "Compile error: %s\n%!" msg
      end;
      iter ()
    end
  in
    iter ()

let repl () =
  let lexbuf = Lexing.from_channel stdin in
  let lex = Lexer.lex "<repl>" in
  let prompt = ">> " in
  let rec iter () =
    begin
      Printf.printf "%s%!" prompt;
      begin
        try
          match Parser.repl lex lexbuf with
            | None -> iter ()
            | Some ast ->
                let ir = Ir.convert ast in
                let ir2 = Ir2.convert ir in
                  Eval.eval_form ir2
        with
          | Lexer.Lexer_error err ->
              handle_lex_error lexbuf err
          | Parser.Error _ ->
              handle_parser_error lexbuf
          | Parse_error msg ->
              Printf.printf "Parse error: %s\n%!" msg
          | Compile_error msg ->
              Printf.printf "Compile error: %s\n%!" msg
      end;
      iter ()
    end
  in
    iter ()

let end_program () =
  begin
    Printf.printf "  \n%!";  (* an ugly hack to make exiting look clean *)
    exit 0
  end

let _ = 
  match Sys.argv with
    | [| _; "-test" |] ->
      begin
        try 
          repl_test ()
        with End_of_file -> end_program ()
      end
    | [| _ |] ->
      begin
        try 
          repl ()
        with End_of_file -> end_program ()
      end
    | _ -> 
      begin
        Printf.eprintf "usage: %s\n%!" progname;
        exit 1;
      end

