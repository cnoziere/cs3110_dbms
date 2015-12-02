open Async.Std
open Types

(* [table_to_json db t] converts the table named t of database db
 * into a JSON value.
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val table_to_json: database -> string -> Types.result

(* [table_to_file db t] writes the JSON value that represents the
 * table t of database db to file.
 * Writing to file is done asynchronously so a deferred is returned.
 * The deferred becomes determined with value Success if the operation
 * was successfully completed. Otherwise it becomes determined with
 * Failure some_error_message.
 * The file is written to the path './dbname/t.json' where dbname
 * is the name of db. *)
val table_to_file: database -> string -> Types.result Deferred.t

(* [databse_to_json db] converts db into a JSON value *)
val database_to_json: database -> Yojson.Basic.json

(* [database_to_file db] writes db file.
 * The database is written to the file './dbname/dbname.json' where
 * dbname is the name of db. *)
val database_to_file: database -> unit

(* [watch_for_update db] writes a table to file when it has been modified
 * or writes the current database to file when a table is added/removed. *)
val watch_for_update: database -> unit