open Types

(**
 * Starts REPL to infinitely loop to read user input, and calls other
 * Parser functions to evaluate and print.
 *)
val start_repl : unit -> unit

(**
 * Matches user input to find appropriate command to call from ReadJSON or
 * Operation. Used when a database is initialized.
 *)
val evaluate_db : database -> string -> (result * bool)

(**
 * Matches user input to find appropriate command to call from ReadJSON or
 * Operation. Used when no database has been initialized.
 *)
val evaluate : string -> (result * bool)

(**
 * Display results with a readable format in the terminal.
 *)
val print_result : result -> unit
