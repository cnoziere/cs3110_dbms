open Async.Std

(* type database = {name:string; tables: table Tst.tree; updated : database Ivar.t} *)

(* writes a JSON value to the specified file name *)
val json_to_file: Yojson.Basic.json -> string -> unit

(* checks to see if the database has been updated and if so
writes the database to file *)
val watch_for_update: InternalRep.database -> unit

(* converts a table into a JSON value *)
val table_to_json: InternalRep.table -> Yojson.Basic.json

(* converts a database into a JSON value *)
val database_to_json: InternalRep.database -> Yojson.Basic.json