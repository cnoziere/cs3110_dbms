open Yojson.Basic
open InternalRep
open Types

let to_string (lst : string list) =
  List.map (fun x -> `String x) lst

(* converts a table into a JSON value *)
let table_to_json (tablename : string) =
  let open Async.Std in
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
      if Ivar.is_full result then
      Failure ("Cannot convert table " ^ tablename ^ " to json format")
      else let table = `Assoc [("tableName", `String tablename);
                       ("columnNames", `List (to_string column_names));
                       ("columns", `List columns')] in
      Json table
  | _ -> Failure ("No table named " ^ tablename ^ " in current database")

(* writes a table JSON value to the specified file name *)
(* attention: Writer.write overwrites the contents of the previous file *)
let table_to_file (tablename : string) =
  let open Async.Std in
  let dbname = get_name () in
  let name = dbname ^ "_" ^ tablename ^ ".json" in
  match table_to_json tablename with
  | Json table -> Writer.open_file "tmp.json" >>=
                  (fun t -> Writer.write t (pretty_to_string table);
                  Writer.close t >>=
                  (fun () -> let _ = Sys.rename "tmp.json" name in
                  return Success))
  | _ -> let err_string = "Cannot write table " ^ tablename ^
                          " from database " ^ dbname ^ " to file" in
         return (Failure err_string)

(* converts a database into a JSON value *)
let database_to_json () =
  let dbname = get_name () in
  let tablenames = get_table_names () in
  `Assoc [("dbName", `String dbname);
  ("tables", `List (to_string tablenames))]

  (* let err_string = ("Cannot convert database " ^ dbname ^ " to json format") in
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
  Json db *)

(* writes a JSON value to the specified file name *)
(* attention: Writer.write overwrites the contents of the previous file *)
let database_to_file () =
  let db = database_to_json () in
  let dbname = get_name () in
  let d1 = let open Async.Std in
           Writer.open_file "tmp.json" >>=
          (fun t -> Writer.write t (pretty_to_string db); Writer.close t) in
  Async.Std.upon d1 (fun () ->
    let name = dbname ^ ".json" in
    if Sys.file_exists name then Sys.remove name else ();
    Sys.rename "tmp.json" name)

  (* let dbname = get_name () in
  match database_to_json () with
  | Json db -> Writer.open_file "tmp.json" >>=
               (fun t -> Writer.write t (Yojson.Basic.to_string db);
               Writer.close t >>=
               (fun () -> let _ = Sys.rename "tmp.json" (dbname ^ ".json") in
               return Success))
  | _ -> return (Failure ("Cannot write database " ^ dbname ^ " to file")) *)

(* checks to see if the database has been updated and if so
writes the database to file *)
let rec watch_for_update () =
  failwith "TODO"
  (* upon (updated ()) (fun () -> watch_for_update ();
  ignore (database_to_file ())) *)


(*
* Call table_to_file everytime a pre-existing table is modified.
* Call databse_to_file everytime a new table is created.
*)