open Types

type database = {tables: (string * table) list; updated : database Ivar.t}
type table = column tree ref
type column = key tree ref
type key = int

(* converts a table into a JSON value *)
let table_to_JSON t = failwith "TODO"

(* converts a database into a JSON value *)
let database_to_JSON d = failwith "TODO"

(* writes a JSON value to the specified file name *)
let JSON_to_file json = failwith "TODO"

(* checks to see if the database has been updated and if so
writes the database to file *)
let watch_for_update d = failwith "TODOD"