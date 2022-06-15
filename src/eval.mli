(** Evaluator. *)

val add_to_env : Types.id -> Types.expr -> unit

val desugar : Types.expr -> Types.expr2

val eval_expr2 : Types.expr2 -> Types.expr2

val eval_form : Types.form -> Types.expr2 option

val eval_form_print : Types.form -> unit

val load_file : string -> unit Parser_utils.result
