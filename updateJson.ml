open Yojson.Basic
open InternalRep
open Types
open Async.Std

let to_string (lst : string list) =
  List.map (fun x -> `String x) lst

(* converts a table into a JSON value *)
let table_to_json (db : database) (tablename : string) =
  let open Async.Std in
  let dbname = db.name in
  match (get_column_names db tablename) with
  | ColNames column_names ->
      let alwaystrue = fun x -> true in
      let result = Ivar.create () in
      let f = fun c -> get_column_vals db tablename c alwaystrue in
      let columns = List.map f column_names in
      let f' = fun c -> match c with
                        | Column v -> `List (to_string v)
                        | _ -> if Ivar.is_empty result
                               then Ivar.fill result ()
                               else (); `Null in
      let columns' = List.map f' columns in
      if Ivar.is_full result then
      Failure ("Cannot convert table " ^ tablename ^ "from database " ^ dbname ^
        " to json format")
      else let table = `Assoc [("tableName", `String tablename);
                       ("columnNames", `List (to_string column_names));
                       ("columns", `List columns')] in
      Json table
  | _ -> failwith ("This should never pattern match")

(* writes a table JSON value to the specified file name *)
(* attention: Writer.write overwrites the contents of the previous file *)
let write_to_file json path =
  let data = pretty_to_string json in
  Writer.open_file "tmp.json" >>=
  fun t -> Writer.write t data; Writer.close t >>=
  fun _ -> Writer.open_file path >>=
  fun t' -> Writer.write t' data; Writer.close t'

let table_to_file (db : database) (tablename : string) =
  let dbname = db.name in
  let path =  "./" ^ dbname ^ "/" ^ tablename ^ ".json" in
  match table_to_json db tablename with
  | Json table -> write_to_file table path
  | _ -> failwith ("This should never pattern match")

(* converts a database into a JSON value *)
let database_to_json (db : database) =
  let tablenames = get_table_names db in
  `Assoc [("dbName", `String db.name);
  ("tables", `List (to_string tablenames))]

(* writes a JSON value to the specified file name *)
(* attention: Writer.write overwrites the contents of the previous file *)
let database_to_file (db : database) =
  let json = database_to_json db in
  let dbname = db.name in
  let path  = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
  write_to_file json path

(* checks to see if the database has been updated and if so
writes the database to file *)
let rec watch_for_update (db : database) =
  Async.Std.upon (updated db)
  (fun (db', tablename) -> watch_for_update db';
    let check = fun x -> List.mem tablename (InternalRep.get_table_names x) in
    match (check db, check db') with
    | (true, true)   -> ignore (table_to_file db' tablename)     (* modified table *)
    | (true, false)  -> ignore (Sys.remove ("./" ^ db.name ^ "/" ^ tablename ^ ".json") >>=
                        fun () -> database_to_file db')          (* removed table *)
    | (false, true)  -> ignore (table_to_file db' tablename >>= fun _ ->
                        database_to_file db')                    (* added table *)
    | (false, false) -> ())