open Yojson.Basic
open Async.Std
open Types

(* [table_to_json db t] converts the table named t of database db
 * into a JSON value. *)
val table_to_json: database -> string -> json

(* [databse_to_json db] converts db into a JSON value *)
val database_to_json: database -> json

(* [table_to_file db t] writes the JSON value that represents
 * table t of database db to file.
 * Writing to file is done asynchronously so a deferred is returned.
 * The file is written to the path './dbname/t.json' where dbname
 * is the name of db. *)
val table_to_file: database -> string -> unit Deferred.t

(* [database_to_file db] writes database db file.
 * The database is written to the path './dbname/dbname.json'
 * where dbname is the name of db. *)
val database_to_file: database -> unit Deferred.t

(* This function implements the observer pattern from Rec 18
 * [watch_for_update db] performs one of the following:
 * 1) if a table from db has been modified, it writes the new table to file
 * 2) if a table has been added/removed, it writes the updated database to file *)
val watch_for_update: database -> unit Deferred.t