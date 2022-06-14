(** Evaluator. *)

val eval_expr2 : Types.expr2 -> Types.expr2

val eval_form : Types.form -> Types.expr2 option

val add_to_env : Types.id -> Types.expr -> unit

val desugar : Types.expr -> Types.expr2

val trace : bool ref
