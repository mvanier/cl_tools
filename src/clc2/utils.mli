(** Utilities. *)

type id = string
[@@deriving sexp_of]

exception Parse_error of string

val parse_err : string -> 'a
