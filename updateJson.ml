(* converts a table into a JSON value *)
let table_to_JSON t = failwith "TODO"

(* converts a database into a JSON value *)
let database_to_JSON d = failwith "TODO"
  List.iter d.tables

(* writes a JSON value to the specified file name *)
let JSON_to_file json = failwith "TODO"
(* Yojson.Basic to_file string json*)

let updated = Ivar.read

(* checks to see if the database has been updated and if so
writes the database to file *)
let watch_for_update d =
  upon (updated d) (fun d' -> watch_for_update d';
    JSON_to_file (database_to_JSON d))