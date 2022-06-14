(** Evaluator. *)

val evaluate : Types.expr2 -> Types.expr2

val add_to_env : Types.id -> Types.expr -> unit

val desugar : Types.expr -> Types.expr2

val trace : bool ref
