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
let database_to_json (dbname : string) =
  failwith "TODO"
  (* let err_string = ("Cannot convert database " ^ dbname ^ " to json format") in
  let tablenames = ... in
  let result = Ivar.create () in
  let f = fun t -> (let table = table_to_json t in
                    let () = (match table with
                              | Json _ -> ()
                              | _ -> if Ivar.is_empty result
                                     then Ivar.fill result ()
                                     else ()) in
                    table) in

    let tables = List.map f tablenames in
    if Ivar.is_full result then Failure err_string
    else let db = `Assoc [("dbName", `String dbname); ("tables", `List tables)] in
    Json db *)

(* writes a JSON value to the specified file name *)
let database_to_file (dbname : string) =
  match database_to_json dbname with
  | Json db -> Yojson.Basic.to_file dbname db; Success
  | _ -> Failure ("Cannot write database " ^ dbname ^ " to file")

(* checks to see if the database has been updated and if so
writes the database to file *)
let watch_for_update () = failwith "TODO"
  (* let (update, dbname) = updated () in
  upon (update) (fun _ -> watch_for_update ();
  match database_to_file dbname with
  | Success -> ()
  | _ -> failwith ("Cannot write database " ^ dbname ^ " to file")) *)

  (* let watch_for_update d = failwith "TODO"
     upon (Ivar.read d) (fun d' -> watch_for_update d';
    (database_to_file d)) *)

(* WHAT SHOULD RETURN TYPE OF WATCH_FOR_UPDATE BE? *)