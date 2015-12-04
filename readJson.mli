(* This module handles the creation of databases and tables from files *)
open Types
open Async.Std

(* [load_db dbname] loads the json file from path
 * './dbname/dbname.json' and creates a database from the file
 * contents
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val load_db: string -> result

(* [ok_to_create_database d] returns true if no database with
 * name d currently exists on disk and false otherwise *)
val ok_to_create_db: string -> bool Deferred.t

(* [drop_db d] deletes all files pertaining to database
 * db from disk.
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val drop_db : string -> result Deferred.t