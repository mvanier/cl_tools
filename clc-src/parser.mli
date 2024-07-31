(** Parser. *)
open Parser_utils

(** Parse a single toplevel form from an input buffer. *)
val parse : input -> Types.form result

(** Try to parse a single toplevel form.
    Return None if there are none. *)
val parse_opt : input -> Types.form option result

(** Parse multiple toplevel forms, separated by whitespace,
    from an input buffer. *)
val parse_many : input -> Types.form list result

(** Parse toplevel forms from a file. 
    The first argument is the filename. *)
val parse_file : string -> Types.form list result

(** Utility to test a parser. *)
val parse_test : 'a parser -> string -> 'a result

