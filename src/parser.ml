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

let parse_keyword_def =
  parse_token "keyword def" (TOK_DEF dummy_loc)

let parse_cmd_name input =
  match peek_token input with
    | TOK_CMD (l, s) -> (skip_token input; Ok (l, s))
    | TOK_EOF        -> (skip_token input; Incomplete)
    | t              -> err t "command name"

let parse_int input =
  match peek_token input with
    | TOK_INT (l, i) -> (skip_token input; Ok (l, i))
    | TOK_EOF        -> (skip_token input; Incomplete)
    | t              -> err t "non-negative integer"

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
       [(let* (l, i) = parse_prim in return (l, Prim (prim_of_id i)));
        (let* (l, i) = parse_comb in return (l, Comb i));
        (let* (l, i) = parse_var  in return (l, Var i))]))
  input

and prim_of_id = function
  | "S" -> S
  | "K" -> K
  | "I" -> I
  | "B" -> B
  | "C" -> C
  | "W" -> W
  | "M" -> M
  | _   -> failwith "unknown primitive"

and parse_expr input =
  (wrap_err "expression"
     (choice
       [parse_list;
        let* (_, a) = parse_atom in return (Atom a)]))
  input

and parse_exprs input =
  (wrap_err "list of expressions"
    (many parse_expr))
  input

let parse_def input =
  (wrap_err "top-level definition"
     (let* _      = parse_keyword_def in
      let* (_, c) = parse_comb in
      let* e      = parse_expr in
        return (Def (c, e))))
  input

let parse_cmd input =
  (wrap_err "top-level command"
     (let* (l, name) = parse_cmd_name in
      match name with
        | "c" -> return Curr
        | "n" -> return Norm
        | "s" -> return Step
        | "u" -> return Undo
        | "q" -> return Quit
        | "sc" ->
          let* (_, a) = parse_atom in
            begin
              match a with
                | Var _ -> fail l "invalid command"
                | _ -> return (StepC a)
            end
        | "scn" ->
          let* (_, a) = parse_atom in
            begin
              match a with
                | Var _ -> fail l "invalid command"
                | _ ->
                  let* (_, n) = parse_int in
                    return (StepCN (a, n))
            end
        | "ms" ->
           let* (_, i) = parse_int in
             return (MaxSteps i)
        | _ -> fail l "invalid command"))
  input

let parse_form input =
  (wrap_err "top-level form"
     (choice
       [parse_def;
        (let* c = parse_cmd in return (Cmd c));
        (let* e = parse_expr in return (Expr e))]))
  input

let parse = parse_form

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

