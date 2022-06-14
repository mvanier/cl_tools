open Types
open Lexer_utils
open Loc
open Parser_utils

(*
 * Parsers.
 * NOTE: Parsers which fail on the first token don't consume that token.
 *)

(* The following parsers attempt to consume a single specific token.
   If they hit TOK_EOF, the input is incomplete.
   Otherwise, they signal an error, but nothing is consumed. *)

let parse_token label token input =
  match peek_token input with
    | TOK_EOF ->
      begin
        skip_token input;
        Incomplete
      end

    | t when token_eq_constructor t token ->
      begin
        skip_token input;
        Ok (loc_of_token t)
      end

    | t -> err t label

(* NOTE: I pass in a manufactured token with a bogus location
   because I want to test that the real token has the right constructor,
   and the location is irrelevant. *)

let parse_open_paren =
  parse_token "open parenthesis" (TOK_LPAREN dummy_loc)

let parse_close_paren =
  parse_token "close parenthesis" (TOK_RPAREN dummy_loc)

let parse_prim input =
  match peek_token input with
    | TOK_PRIM (l, i) -> (skip_token input; Ok (l, i))
    | TOK_EOF         -> (skip_token input; Incomplete)
    | t               -> err t "primitive"

let parse_comb input =
  match peek_token input with
    | TOK_COMB (l, i) -> (skip_token input; Ok (l, i))
    | TOK_EOF         -> (skip_token input; Incomplete)
    | t               -> err t "combinator"

let parse_var input =
  match peek_token input with
    | TOK_VAR (l, i) -> (skip_token input; Ok (l, i))
    | TOK_EOF        -> (skip_token input; Incomplete)
    | t              -> err t "identifier"

(* NOTE: The eta-expansions are to make `let rec` happy. *)
let rec parse_list input =
  (wrap_err "list"
     (let* _  = parse_open_paren  in
      let* es = parse_exprs       in
      let* _  = parse_close_paren in
          return (List es)))
  input

and parse_atom input =
  (wrap_err "atom"
     (choice
       [(let* (_, i) = parse_prim in return (Prim (prim_of_id i)));
        (let* (_, i) = parse_comb in return (Comb i));
        (let* (_, i) = parse_var  in return (Var i))]))
  input

and prim_of_id = function
  | "S" -> S
  | "K" -> K
  | "I" -> I
  | "B" -> B
  | "C" -> C
  | "W" -> W
  | _   -> failwith "Unknown primitive"

and parse_expr input =
  (wrap_err "expression"
     (choice
       [parse_list;
        let* a = parse_atom in return (Atom a)]))
  input

and parse_exprs input =
  (wrap_err "list of expressions"
    (many parse_expr))
  input

let parse = parse_expr

let parse_eof input =
  match peek_token input with
    | TOK_EOF -> (skip_token input; Ok ())
    | t -> err t "eof"

let parse_opt input =
  ((parse_eof >> return None)
    <|> (let* t = parse in return (Some t)))
  input

let parse_many input =
  let rec iter forms =
    match parse_opt input with
      | Ok (Some f)    -> iter (f :: forms)
      | Ok None        -> Ok (List.rev forms)
      | Incomplete     -> Incomplete
      | Error (l, msg) -> Error (l, msg)
  in
    iter []

let parse_file filename =
  let file   = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let input  = make_input filename lexbuf in
  let exprs  = parse_many input in
    begin
      close_in file;
      exprs
    end

(*
 * Testing parsers.
 *)
let parse_test p s =
  let lexbuf = Lexing.from_string s in
  let input = make_input "test" lexbuf in
    p input

