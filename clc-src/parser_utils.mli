(** Parser combinators and other utilities. *)

open Lexer_utils
open Loc

(** Type of parser input. *)
type input = token Input.input

(** Type of parsing results. *)
type 'a result =
  | Ok of 'a
  | Error of loc * string
  | Incomplete

(** Type of monadic parsers. *)
type 'a parser = input -> 'a result

(** Create an input buffer.  The string is a label. *)
val make_input : string -> Lexing.lexbuf -> input

(** Look at and return the next token from the input buffer.
    Don't consume the token. *)
val peek_token : 'a Input.input -> 'a

(** Skip the next token in the input buffer. *)
val skip_token : 'a Input.input -> unit

(** Create an error result from a token and an error message. *)
val err : token -> string -> 'a result

(** Monadic bind operator for parsers. *)
val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser

(** Monadic bind operator for parsers (alternate syntax). *)
val ( let* ) : ('a parser) -> ('a -> 'b parser) -> 'b parser

(** Monadic sequence operator for parsers. *)
val ( >> ) : 'a parser -> 'b parser -> 'b parser

(** Monadic return operator for parsers. *)
val return : 'a -> 'a parser

(** Monadic fail operator for parsers. *)
val fail : loc -> string -> 'a parser

(** Alternation operator for parsers. *)
val ( <|> ) : 'a parser -> 'a parser -> 'a parser

(** Alternation operator for parser lists. *)
val choice : 'a parser list -> 'a parser

(** Change the error message from a parser if any. *)
val wrap_err : string -> 'a parser -> 'a parser

(** Parse zero or more of a parser. *)
val many : 'a parser -> 'a list parser

(** Test a parser given string input. *)
val parse_test : (input -> 'a) -> string -> 'a

