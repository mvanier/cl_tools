open Sexplib.Conv

type id = string
[@@deriving sexp_of]

exception Parse_error of string
