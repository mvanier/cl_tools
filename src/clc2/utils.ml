open Sexplib.Conv

type id = string
[@@deriving sexp_of]

exception Parse_error of string

let parse_err msg = raise (Parse_error msg)

exception Compile_error of string

let compile_err msg = raise (Compile_error msg)
