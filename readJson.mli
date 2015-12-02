(* This module handles the creation of databases and tables from files *)
open Types
open Yojson.Basic

(* [read_db_json d] creates the database specified by contents of file d
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val load_db: string -> result

(* [create_database dbname d] database specified by the JSON value d
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val create_database: string -> Yojson.Basic.json -> result

(* [create_full_table t] creates and fills a table as specified
   by the JSON value t
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message *)
val create_full_table: database -> string -> Yojson.Basic.json -> result

(* [ok_to_create_database d] return true if no other database with
 * name d exists and false otherwise *)
val ok_to_create_database: string -> bool