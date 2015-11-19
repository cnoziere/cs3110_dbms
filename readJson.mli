(* This module handles the creation of databases and tables from files *)
open Types

(* create the database specified by contents of the file with the given
filename *)
(* returns false if the file cannot be found or a database with that
name already exists *)
val read_JSON: string -> result

(* creates the database specified by the JSON value *)
(* returns Success if creation of the database was successful *)
(* otherwise returns Failure if the JSON value could not be parsed *)
val create_database: Yojson.Basic.json -> result

(* creates and fills the table as specified by the JSON value *)
(* returns Success if creation of the table was successful *)
(* otherwise returns Failure if the JSON value could not be parsed *)
val create_full_table: Yojson.Basic.json -> result