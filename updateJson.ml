open Yojson.Basic
open InternalRep
open Async.Std
open Types

let to_string (lst : string list) =
  List.map (fun x -> `String x) lst

let table_to_json (db : database) (tablename : string) =
  match (get_column_names db tablename) with
  | ColNames column_names ->
      let alwaystrue = fun x -> true in
      let f = fun c -> get_column_vals db tablename c alwaystrue in
      let columns = List.map f column_names in
      let f' = fun c -> match c with
                        | Column v -> `List (to_string v)
                        | _ -> failwith ("This should never pattern match") in
      let columns' = List.map f' columns in
      let table = `Assoc [("tableName", `String tablename);
                  ("columnNames", `List (to_string column_names));
                  ("columns", `List columns')] in
      table
  | _ -> failwith ("This should never pattern match")

(* Asynchronously writes the json value to the specified path.
 * First backs data up to tmp.json and then overwrites path.
 * NB: Writer.write overwrites the contents of the file if it
 * previously existed *)
let write_to_file (j : json) (path : string) =
  let data = pretty_to_string j in
  Writer.open_file "tmp.json" >>=
  fun t -> Writer.write t data; Writer.close t >>=
  fun _ -> Writer.open_file path >>=
  fun t' -> Writer.write t' data; Writer.close t'

let table_to_file (db : database) (tablename : string) =
  let path =  "./" ^ db.name ^ "/" ^ tablename ^ ".json" in
  let json_table = table_to_json db tablename in
  write_to_file json_table path

let database_to_json (db : database) =
  let tablenames = get_table_names db in
  `Assoc [("dbName", `String db.name);
  ("tables", `List (to_string tablenames))]

let database_to_file (db : database) =
  let json_db = database_to_json db in
  let dbname = db.name in
  let path  = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
  write_to_file json_db path

(* Implements the observer pattern covered in Rec 18
 * Whenever the database is updated, the deferred becomes determined
 * and the appropriate portion of the database is written to file *)
let rec watch_for_update (db : database) =
  let open Async.Std in
  updated db >>=
  (fun (db', tablename) ->
    let check = fun x -> List.mem tablename (get_table_names x) in
    let path = "./" ^ db.name ^ "/" ^ tablename ^ ".json" in
    let d1 = match (check db, check db') with
    | (true, true)   -> table_to_file db' tablename          (* table modified *)
    | (true, false)  -> Sys.remove path >>=
                        fun () -> database_to_file db'       (* table deleted *)
    | (false, true)  -> table_to_file db' tablename >>=
                        fun _ -> database_to_file db'        (* table added *)
    | (false, false) -> database_to_file db' in
    d1 >>= fun () -> watch_for_update db')