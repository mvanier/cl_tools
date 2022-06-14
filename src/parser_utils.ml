open Lexer_utils
open Lexer
open Loc

(*
 * Types.
 *)

type input = token Input.input

type 'a result =
  | Ok of 'a
  | Error of loc * string
  | Incomplete

(*
 * Input.
 *)

let make_input label lexbuf =
  Input.make_input (fun () -> lex label lexbuf)

let peek_token = Input.peek
let skip_token = Input.skip

let err tok msg =
  Error (loc_of_token tok, "expected " ^ msg)

(*
 * Parsing monad.
 *)

type 'a parser =  input -> 'a result

let (>>=) m f =
  (fun input ->
     match m input with
       | Ok x         -> f x input
       | Error (l, e) -> Error (l, e)
       | Incomplete   -> Incomplete)

let (let*) = (>>=)

let (>>) m1 m2 =
  m1 >>= fun _ -> m2

let return x = fun _ -> Ok x

(*
 * Parser combinators.
 *)

let (<|>) p1 p2 =
  fun input ->
    let next_token = peek_token input in
      match p1 input with
        | Error _ as err ->
          let next_token' = peek_token input in
            if token_eq next_token next_token' then
              (* First parser failed without consuming any tokens.
                 Try the second parser. *)
              p2 input
            else
              (* First parser failed after consuming tokens.
                 Propagate the error. *)
              err
        | r -> r

let rec choice ps =
  match ps with
    | [] -> failwith "no alternatives"
    | [p] -> p
    | p1 :: ps' -> p1 <|> choice ps'

let wrap_err msg p =
  fun input ->
    match p input with
      | Error (l, _) -> Error (l, "expected " ^ msg)
      | r -> r

let rec many p =
  (let* x  = p in
   let* xs = many p in
     return (x :: xs))
  <|> return []

(*
 * Debugging.
 *)

let parse_test p s =
  let lexbuf = Lexing.from_string s in
  let input = make_input "test" lexbuf in
    p input

