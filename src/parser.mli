(** Parser. *)
open Parser_utils

(** Parse a single expression from an input buffer. *)
val parse : input -> Types.expr result

(** Try to parse a single expression.
    Return None if there are none. *)
val parse_opt : input -> Types.expr option result

(** Parse multiple expressions, separated by whitespace,
    from an input buffer. *)
val parse_many : input -> Types.expr list result

(** Parse expressions from a file. 
    The first argument is the filename. *)
val parse_file : string -> Types.expr list result

(** Utility to test a parser. *)
val parse_test : 'a parser -> string -> 'a result

