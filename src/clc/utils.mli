(** Utilities. *)

type id = string
[@@deriving sexp_of]

exception Parse_error of string

exception Compile_error of string

exception Runtime_error of string

val parse_err : string -> 'a

val compile_err : string -> 'a

val runtime_err : string -> 'a
