open Async.Std
open Types

(* [table_to_json t] converts the table with table name t of the current
 * database into a JSON value.
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val table_to_json: database -> string -> Types.result

(* [table_to_file t] writes the JSON value that represents the table t
 * of the current database to file.
 * Writing to file is done asynchronously so a deferred is returned.
 * The deferred becomes determine with value Success if the operation
 * was successfully completed. Otherwise it becomes determined with
 * Failure some_error_message.
 * The file is written to the file 'dbname_tablename.json' where dbname
 * is the name of the database and tablename is the name of the table. *)
val table_to_file: database -> string -> Types.result Deferred.t

(* [databse_to_json ()] converts the current database into a JSON value *)
val database_to_json: database -> Yojson.Basic.json

(* [database_to_file ()] writes the current database to file.
 * The database is written to the file 'dbname.json' where dbname
 * is the name of the database. *)
val database_to_file: database -> unit

(* [watch_for_update ()] writes a table to file when it has been modified
 * or writes the current database to file when a table is added/removed. *)
val watch_for_update: database -> unit