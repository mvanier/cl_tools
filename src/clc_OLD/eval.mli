(** Evaluator. *)

val add_to_env : string -> Types.expr -> unit

val eval_form : Types.form -> unit

val load_file : string -> unit Parser_utils.result
