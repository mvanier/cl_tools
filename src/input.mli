(** Lazy input buffer, for using with lexer. *)

(** Infinite lazy stream type. *)
type 'a stream

(** Input buffer type.  Updates whenever an input is read
    so that the head of the stream is the last thing read. *)
type 'a input = 'a stream ref

(** Create a new input buffer given a function
    which can generates the next value. *)
val make_input : (unit -> 'a) -> 'a input

(** Get the next unprocessed value. *)
val next : 'a input -> 'a

(** Look at but don't process the next value. *)
val peek : 'a input -> 'a

(** Skip the next value.  Normally used after a `peek`. *)
val skip : 'a input -> unit

(** Get the current input stream. *)
val get_stream : 'a input -> 'a stream

(** Rewind to an earlier buffer state. *)
val rewind : 'a input -> 'a stream -> unit

