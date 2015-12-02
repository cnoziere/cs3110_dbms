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
val evaluate_db : database -> string -> (result * bool)

val evaluate : string -> (result * bool)

(**
 * Display results with a readable format in the terminal.
 *)
val print_result : result -> unit
