open Yojson

(* create the database specified by the JSON file with name filename *)
(* returns false if the file cannot be found or a database with that
name already exists *)
val read_JSON: (filename : string) -> bool

(* convert the contents of filename to a JSON value *)
val convert: (filename : string) -> Yojson.Basic.json

(* creates the database specified by the JSON value json *)
(* returns true if creation of the database was successful *)
(* otherwise returns false if the json file could not be parsed *)
val create_database: (json : Yojson.Basic.json) -> bool

(* creates and fills the table as specified by the JSON value json *)
(* returns true if creation of the table was successful *)
(* otherwise returns false if the json file could not be parsed *)
val create_full_table (json : Yojson.Basic.json) -> bool