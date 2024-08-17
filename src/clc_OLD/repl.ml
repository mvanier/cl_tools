open Lexer_utils
open Parser_utils
open Parser

exception Lexer_exit

let print_prompt prompt1 prompt2 s =
  print_string (if s = "" then prompt1 else prompt2)

(* Read and evaluate all top-level forms in the input buffer. *)
let rec repl_loop input eval_fn =
  match parse_opt input with
    | Ok (Some d) ->
      begin
        eval_fn d;
        repl_loop input eval_fn
      end
    | Ok None -> Ok ()
    | Incomplete -> Incomplete
    | Error (l, msg) -> Error (l, msg)

let make_repl primary_prompt secondary_prompt eval_fn error_fn =
  let rec loop s =
    begin
      print_prompt primary_prompt secondary_prompt s;
      let s' = s ^ read_line () ^ "\n" in
        try
          let lexbuf = Lexing.from_string s' in
          let input  = make_input "<repl>" lexbuf in
            match repl_loop input eval_fn with
              | Ok ()          -> loop  ""
              | Incomplete     -> loop s'
              | Error (l, msg) ->
                begin
                  Printf.printf "Error: %s (%s)\n\n%!"
                    msg (Loc.string_of_loc_short l);
                  error_fn ();
                  loop ""
                end
        with
          | Lexer_error (_, LEX_UNTERMINATED_STRING) ->
            loop s'

          | Lexer_error (l, err) ->
            begin
              Printf.printf "Lexer error: %s (%s)\n\n%!"
                (string_of_lex_error err) (Loc.string_of_loc_short l);
              error_fn ();
              loop ""
            end

          | Lexer_exit ->  (* discard rest of input *)
            loop ""
    end
  in
    loop ""

