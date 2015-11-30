open Yojson.Basic
open InternalRep
open Async.Std
open Types

let to_string (lst : string list) =
  List.map (fun x -> `String x) lst

(* converts a table into a JSON value *)
let table_to_json (tablename : string) =
  let err_string = ("Cannot convert table " ^ tablename ^ " to json format") in
  match (get_column_names tablename) with
  | ColNames column_names ->
      let alwaystrue = fun x -> true in
      let result = Ivar.create () in
      let f = fun c -> get_column_vals tablename c alwaystrue in
      let columns = List.map f column_names in
      let f' = fun c -> match c with
                        | Column v -> `List (to_string v)
                        | _ -> if Ivar.is_empty result
                               then Ivar.fill result ()
                               else (); `Null in
      let columns' = List.map f' columns in
      if Ivar.is_full result then Failure err_string
      else let table = `Assoc [("tableName", `String tablename);
                       ("columnNames", `List (to_string column_names));
                       ("columns", `List columns')] in
      Json table
  | _ -> Failure err_string

(* converts a database into a JSON value *)
let database_to_json () =
  let dbname = get_name () in
  let err_string = ("Cannot convert database " ^ dbname ^ " to json format") in
  let tablenames = get_table_names () in
  let result = Ivar.create () in
  let tables = List.map table_to_json tablenames in
  let f' = fun t -> match t with
                    | Json v -> v
                    | _ -> if Ivar.is_empty result
                           then Ivar.fill result ()
                           else (); `Null in
  let tables' = List.map f' tables in
  if Ivar.is_full result then Failure err_string
  else let db = `Assoc [("dbName", `String dbname);
                ("tables", `List tables')] in
  Json db

(* writes a JSON value to the specified file name *)
let database_to_file () =
  let dbname = get_name () in
  match database_to_json () with
  | Json db -> Yojson.Basic.to_file dbname db; Success
  | _ -> Failure ("Cannot write database " ^ dbname ^ " to file")

(* checks to see if the database has been updated and if so
writes the database to file *)
let rec watch_for_update () =
  upon (updated ()) (fun () -> watch_for_update ();
  ignore (database_to_file ()))

(* maybe don't need an ignore here... *)
(* WHAT SHOULD RETURN TYPE OF databse_to_file be? *)