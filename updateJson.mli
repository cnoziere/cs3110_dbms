open Types
open Yojson.Basic

(* converts a table into a JSON value *)
val table_to_JSON: table -> Yosjon.Basic.json

(* converts a database into a JSON value *)
val database_to_JSON: database -> Yosjon.Basic.json

(* writes a JSON value to the specified file name *)
val JSON_to_file: Yojson.Basic.json -> string -> unit

(* checks to see if the database has been updated and if so
writes the database to file *)
val watch_for_update: database -> unit