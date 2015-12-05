open Types
open Async.Std

(**
 * Matches user input to find appropriate command to call from ReadJSON or
 * Operation. Used when a database is initialized.
 *)
val evaluate_db : database -> string -> (result * bool) Deferred.t

(**
 * Matches user input to find appropriate command to call from ReadJSON or
 * Operation. Used when no database has been initialized.
 *)
val evaluate : string -> (result * bool) Deferred.t

(**
 * Display results with a readable format in the terminal.
 *)
val print_result : result -> unit

(**
 * Helper functions for evaluation.
 *)
val exit : string list -> result * bool
val help : string list -> result
val load : string list -> result
val create_database : string list -> result Deferred.t
val create_table : database -> string list -> result
val drop_table : database -> string list -> result
val insert_into : database -> string list -> result
val delete_from : database -> string list -> result
val update : database -> string list -> result
val select : database -> string list -> result
val print : database -> string list -> result

val repl: unit -> unit Deferred.t