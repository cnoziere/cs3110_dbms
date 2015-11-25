open Types

(*
(**
 * Starts REPL to infinitely loop to read user input, and calls other
 * Parser functions to evaluate and print.
 *)
val read_input : unit -> unit

(**
 * Matches input to find appropriate command to call from ReadJSON or
 * Operation.
 *)
val match_command : string -> result

(**
 * Functions to display results with a readable format in the terminal.
 *)

val print_error : result -> unit

val print_values : value list -> unit

val print_table : table -> unit
*)
