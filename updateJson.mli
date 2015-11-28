(* writes a JSON value to the specified file name *)
val json_to_file: Yojson.Basic.json -> string -> unit

(* checks to see if the database has been updated and if so
writes the database to file *)
val watch_for_update: Types.database -> unit

(* converts a table into a JSON value *)
val table_to_json: Types.table -> Yojson.Basic.json

(* converts a database into a JSON value *)
val database_to_json: Types.database -> Yojson.Basic.json