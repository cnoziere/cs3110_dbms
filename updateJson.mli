open Async.Std
open InternalRep

(* type database = {name:string; tables: table Tst.tree; updated : database Ivar.t} *)

(* converts a table into a JSON value *)
val table_to_json: string -> Types.result

(* converts a database into a JSON value *)
val database_to_json: unit -> Types.result

(* writes a JSON value to the specified file name *)
val database_to_file: unit -> Types.result Deferred.t

(* checks to see if the database has been updated and if so
writes the database to file *)
val watch_for_update: unit -> unit