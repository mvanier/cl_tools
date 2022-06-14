(** Read-eval-print loops (REPLs). *)

(** Exception raised when you want the lexer to be flushed. *)
exception Lexer_exit

(** make_repl: generate a read-eval-print loop (REPL).
 *
 *  arguments:
 *    primary_prompt: normal prompt
 *    secondary_prompt: continuation prompt
 *    eval_fn: 
 *      a function taking in an expression and returning nothing
 *    error_fn: called on errors
 *
 *  return value: none
 *)
val make_repl :
  string -> string -> (Types.expr -> unit) -> (unit -> unit) -> unit

