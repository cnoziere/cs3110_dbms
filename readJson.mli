(* This module handles the creation of databases and tables from files *)
open Types
open Yojson.Basic
open Async.Std

(* [load_db dbname] loads the json file from path
 * './dbname/dbname.json' and creates a database from the file
 * contents
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val load_db: string -> result

(* [create_database dbname j] creates the database with name
 * dbname and contents as specified by the JSON value j
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val create_db: string -> json -> result

(* [ok_to_create_database d] returns true if no database with
 * name d currently exists on disk and false otherwise *)
val ok_to_create_db: string -> bool Deferred.t

(* [create_full_table db tablename j] creates the table with
 * name tablename and contents as specified by the JSON value j.
 * The table is added to database db.
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val create_full_table: database -> string -> json -> result

(* [drop_db d] deletes all files pertaining to database
 * db from disk.
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
(* val drop_db : database -> result *)