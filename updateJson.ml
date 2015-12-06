open Yojson.Basic
open InternalRep
open Async.Std
open Testing
open Types

let to_string (lst : string list) =
  List.map (fun x -> `String x) lst

(* [table_to_json db t] converts the table named t of database db
 * into a JSON value.
 * val table_to_json: database -> string -> json *)
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
  Writer.open_file "tmp.json" >>= fun t ->
    Writer.write t data; Writer.close t >>= fun _ ->
      Writer.open_file path >>= fun t' ->
        Writer.write t' data; Writer.close t'

(* [table_to_file db t] writes the JSON value that represents
 * table t of database db to file.
 * Writing to file is done asynchronously so a deferred is returned.
 * The file is written to the path './dbname/t.json' where dbname
 * is the name of db.
 * val table_to_file: database -> string -> unit Deferred.t *)
let table_to_file (db : database) (tablename : string) =
  let path =  "./" ^ db.name ^ "/" ^ tablename ^ ".json" in
  let json_table = table_to_json db tablename in
  write_to_file json_table path

(* [databse_to_json db] converts db into a JSON value
 * val database_to_json: database -> json *)
let database_to_json (db : database) =
  let tablenames = get_table_names db in
  `Assoc [("dbName", `String db.name);
  ("tables", `List (to_string tablenames))]

(* [database_to_file db] writes database db file.
 * The database is written to the path './dbname/dbname.json'
 * where dbname is the name of db.
 * val database_to_file: database -> unit Deferred.t *)
let database_to_file (db : database) =
  let json_db = database_to_json db in
  let dbname = db.name in
  let path  = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
  write_to_file json_db path

let rec watch_for_update_helper (db : database) =
  let open Async.Std in
  updated db >>= fun (db', tablename) ->
    let check = fun x -> List.mem tablename (get_table_names x) in
    let path = "./" ^ db.name ^ "/" ^ tablename ^ ".json" in
    let d1 = match (check db, check db') with
             | (true, true)   -> table_to_file db' tablename (* table modified *)
             | (true, false)  -> Sys.remove path >>= fun () ->
                                   database_to_file db'      (* table deleted *)
             | (false, true)  -> table_to_file db' tablename >>= fun _ ->
                                   database_to_file db'      (* table added *)
             | (false, false) -> database_to_file db' in
    return (d1, db')

(* Implements the observer pattern covered in Rec 18
 * Whenever the database is updated, the deferred becomes determined
 * and the appropriate portion of the database is written to file *)
let rec watch_for_update (db : database) =
  let open Async.Std in
  watch_for_update_helper db >>= fun (d1, db') ->
  d1 >>= fun () -> watch_for_update db'

(******************************************************************************)
(********************************UNIT TESTS************************************)
(******************************************************************************)

let j = `Assoc [("dbName", `String "RJtest");
        ("tables", `List [`String "t1"])]

let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"]);
   ("columns",
    `List
      [`List [`String "Asta"; `String "Amanda"];
       `List [`String "13"; `String "14"]])]

let t2 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"]);
   ("columns",
    `List
      [`List [`String "Amanda"; `String "Asta"];
       `List [`String "14"; `String "13"]])]

let t3 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Age"; `String "Name"]);
   ("columns",
    `List
      [`List [`String "14"; `String "13"];
      `List [`String "Amanda"; `String "Asta"]])]

let t4 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Age"; `String "Name"]);
   ("columns",
    `List
      [`List [`String "13"; `String "14"];
      `List [`String "Asta"; `String "Amanda"]])]

TEST_MODULE "table_to_json" = struct
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = match ReadJson.load_db "RJtest" with
         | Success db -> let t' = table_to_json db "t1" in
                         (t' = t1 || t' = t2 || t' = t3 || t' = t4)
         | _ -> false
end

TEST_MODULE "database_to_json" = struct
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = match ReadJson.load_db "RJtest" with
         | Success db -> j = (database_to_json db)
         | _ -> false
end

TEST_MODULE "table_to_file" = struct
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = match ReadJson.load_db "RJtest" with
         | Success db -> let _ = Thread_safe.block_on_async
                                 (fun () -> create_dir ()) in
                         let b1 = test_async_eq
                                  (Sys.file_exists_exn "RJtest/t1.json")
                                  false in
                         let _ = Thread_safe.block_on_async
                                 (fun () -> table_to_file db "t1") in
                         let b2 = test_async_eq
                                  (Sys.file_exists_exn "RJtest/t1.json") true in
                         let t' = Yojson.Basic.from_file "RJtest/t1.json" in
                         let b3 = (t' = t1 || t' = t2 || t' = t3 || t' = t4) in
                         b1 && b2 && b3
         | _ -> false
end

TEST_MODULE "database_to_file" = struct
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = match ReadJson.load_db "RJtest" with
         | Success db -> let _ = Thread_safe.block_on_async
                                 (fun () -> create_dir ()) in
                         let _ = Thread_safe.block_on_async
                                 (fun () -> Sys.remove "RJtest/RJtest.json") in
                         let b1 = test_async_eq
                                  (Sys.file_exists_exn "RJtest/RJtest.json")
                                  false in
                         let _ = Thread_safe.block_on_async
                                 (fun () -> database_to_file db) in
                         let b2 = test_async_eq
                                  (Sys.file_exists_exn "RJtest/RJtest.json")
                                  true in
                         let t' = Yojson.Basic.from_file "RJtest/RJtest.json" in
                         let b3 = t' = j in
                         b1 && b2 && b3
         | _ -> false
end

let k = `Assoc [("dbName", `String "RJtest");
        ("tables", `List [`String "t1"])]

let k1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"]);
   ("columns",
    `List
      [`List [`String "Asta"; `String "Amanda"]])]

let k1' = `Assoc [("tableName", `String "t1"); ("columnNames", `List [`String "Name"]);
   ("columns",
    `List [`List [`String "Asta"; `String "Amanda"; `String "Constance"]])]

let v1 = `Assoc [("tableName", `String "t2");
   ("columnNames", `List [`String "Name"; `String "Age"]);
   ("columns", `List [`List []; `List []])]

let v2 = `Assoc [("tableName", `String "t2");
   ("columnNames", `List [`String "Age"; `String "Name"]);
   ("columns", `List [`List []; `List []])]

let k' = `Assoc [("dbName", `String "RJtest");
        ("tables", `List [`String "t1"; `String "t2"])]

TEST_MODULE "watch_for_update" = struct
  (* table modified *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" k
  let () = Yojson.Basic.to_file "RJtest/t1.json" k1
  let b4 = match ReadJson.load_db "RJtest" with
         | Success db -> let res = InternalRep.add_row db "t1" ["Name"] ["Constance"] in
                         watch_for_update_helper db >>= fun (d1, _) -> d1 >>= fun _ ->
                         (match res with
                          | Success db' -> let b1 = db <> db' in
                                           let b2 = (Yojson.Basic.from_file "RJtest/t1.json") = k1' in
                                           return (b1 && b2)
                          | _ -> return false)
        | _ -> return false
  TEST = test_async_eq b4 true

 (* table added *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" k
  let () = Yojson.Basic.to_file "RJtest/t1.json" k1
  let b4 = match ReadJson.load_db "RJtest" with
         | Success db -> let res = Operation.create_table db "t2" ["Name"; "Age"] in
                         watch_for_update_helper db >>= fun (d1, _) -> d1 >>= fun _ ->
                         (match res with
                          | Success db' -> let b1 = db <> db' in
                                           let b2 = (Yojson.Basic.from_file "RJtest/t2.json") = v1 in
                                           let b3 = (Yojson.Basic.from_file "RJtest/t2.json") = v2 in
                                           let b4 = (Yojson.Basic.from_file "RJtest/RJtest.json") = k' in
                                           return (b1 && (b2 || b3) && b4)
                          | _ -> return false)
        | _ -> return false
  TEST = test_async_eq b4 true
end