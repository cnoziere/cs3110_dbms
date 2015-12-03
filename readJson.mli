(* This module handles the creation of databases and tables from files *)
open Types
open Yojson.Basic
open Async.Std

(* [load_db dbname] creates the database specified by path
 * './dbname/dbname.json'
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val load_db: string -> result

(* [create_database dbname json] creates the database with name
 * dbname and contents as specified by json
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val create_db: string -> json -> result

(* [create_full_table db tablename json] creates the table with
 * name tablename and contents as specified by json. The table
 * is added to database db.
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val create_full_table: database -> string -> json -> result

(* [ok_to_create_database d] returns true if no database with
 * name d currently exists and false otherwise *)
val ok_to_create_database: string -> bool Deferred.t