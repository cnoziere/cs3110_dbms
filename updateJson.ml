open Yojson.Basic
open InternalRep
open Types
open Async.Std

let to_string (lst : string list) =
  List.map (fun x -> `String x) lst

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

(* asynchronously writes the json value to the specified path
 * NB: Writer.write overwrites the contents of the file if it
 * previously existed *)
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

let database_to_json (db : database) =
  let tablenames = get_table_names db in
  `Assoc [("dbName", `String db.name);
  ("tables", `List (to_string tablenames))]

let database_to_file (db : database) =
  let json = database_to_json db in
  let dbname = db.name in
  let path  = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
  write_to_file json path

(* implements the observer pattern covered in Rec 18
 * whenever the database is updated, the deferred becomes determined
 * and the appropriate portion of the database is written to file *)
let rec watch_for_update (db : database) =
  Async.Std.upon (updated db)
  (fun (db', tablename) -> watch_for_update db';
    let check = fun x -> List.mem tablename (InternalRep.get_table_names x) in
    let path = "./" ^ db.name ^ "/" ^ tablename ^ ".json" in
    match (check db, check db') with
    | (true, true)   -> ignore (table_to_file db' tablename)  (* modified table *)
    | (true, false)  -> ignore (Sys.remove path >>=
                        fun () -> database_to_file db')       (* removed table *)
    | (false, true)  -> ignore (table_to_file db' tablename >>= fun _ ->
                        database_to_file db')                 (* added table *)
    | (false, false) -> ())