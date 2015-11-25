open Types

(**
 * Starts REPL to infinitely loop to read user input, and calls other
 * Parser functions to evaluate and print.
 *)
val start_repl : unit -> unit

(**
 * Matches user input to find appropriate command to call from ReadJSON or
 * Operation.
 *)
val evaluate : string -> (result * bool)

(*
(**
 * Functions to display results with a readable format in the terminal.
 *)

val print_error : result -> unit

val print_values : value list -> unit

val print_table : table -> unit
*)
