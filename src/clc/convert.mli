(** Conversion from lambda terms to combinators. *)

val convert : Ast.converter -> Ast.lambda -> Ir2.expr

