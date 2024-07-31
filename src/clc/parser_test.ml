open Clclib

open Types
open Lexer_utils
open Loc
open Parser_utils
open Parser

let test_parser filename lexbuf =
  let input = make_input filename lexbuf in
  let rec iter () =
    try
      match parse_opt input with
        | Ok (Some f) ->
          begin
            Printf.printf "%s\n" (string_of_form f);
            iter ()
          end
        | Ok None -> ()
        | Error (l, msg) ->
            Printf.printf "ERROR: %s at %s\n" msg (string_of_loc l)
        | Incomplete ->
          Printf.printf "Incomplete parse.\n"
    with
      | Lexer_error (l, s) ->
        begin
          Printf.printf "LEXER ERROR: %s (%s)\n\n"
            (string_of_lex_error s)
            (Loc.string_of_loc_short l);
        end
  in
    iter ()

let _ =
  let args = Sys.argv in
    match args with
      | [| _; filename |] ->
        let file = open_in filename in
        let lexbuf = Lexing.from_channel file in
          begin
            test_parser filename lexbuf;
            close_in file
          end
      | _ ->
        let usagestr = Printf.sprintf "usage: %s filename\n" args.(0) in
          print_string usagestr

