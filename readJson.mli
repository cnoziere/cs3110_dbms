(* This module handles the creation of databases and tables from files *)
open Types

(* [read_JSON file] creates the database specified by contents of file *)
(* returns Failure if the file could not be found or could not successfully
be parsed *)
val read_JSON: string -> result

(* [create_databse d] database specified by the JSON value d *)
(* returns Success if creation of the database was successful *)
(* otherwise returns Failure if the JSON value could not be parsed *)
val create_database: Yojson.Basic.json -> result

(* [create_full_table t] creates and fills a table as specified
by the JSON value t *)
(* returns Success if creation of the table was successful *)
(* otherwise returns Failure if the JSON value could not be parsed *)
val create_full_table: Yojson.Basic.json -> result