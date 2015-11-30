open Yojson.Basic
open InternalRep
open Async.Std
open Types

let to_string (lst : string list) =
  List.map (fun x -> `String x) lst

(* converts a table into a JSON value *)
let table_to_json (tablename : string) =
  let err_string = ("Cannot convert " ^ tablename ^ " to json format") in
  match (get_column_names tablename) with
  | ColNames column_names ->
      let alwaystrue = fun x -> true in
      let result = Ivar.create () in
      let f = fun c -> (let vals = get_column_vals tablename c alwaystrue in
                       let () = (match vals with
                                 | Column _ -> ()
                                 | _ -> if Ivar.is_empty result
                                        then Ivar.fill result ()
                                        else ()) in
                       vals) in

      let columns = List.map f column_names in
      if Ivar.is_full result then Failure err_string
      else let f' = fun c -> match c with
                             | Column v -> `List (to_string v)
                             | _ -> failwith "This will never pattern match" in
      let columns' = List.map f' columns in

      let table = `Assoc [("tableName", `String tablename);
      ("columnNames", `List (to_string column_names));
      ("columns", `List columns')] in
      Json table

  | _ -> Failure err_string

(* converts a database into a JSON value *)
let database_to_json (database : string) =
  failwith "TODO"
  (* let r = ref [] in
  traverse_database "" r database;
  `Assoc [("dbName", `String databasename);
  ("tables", `List !r)] *)

(* writes a JSON value to the specified file name *)
let json_to_file json = failwith "TODO"
(* Yojson.Basic to_file string json*)

(* checks to see if the database has been updated and if so
writes the database to file *)
let watch_for_update d = failwith "TODO"
  (* upon (Ivar.read d) (fun d' -> watch_for_update d';
    json_to_file (database_to_json d)) *)