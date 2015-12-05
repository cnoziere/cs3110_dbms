(* This module handles the creation of databases and tables from json files *)
open Types
open InternalRep
open Yojson.Basic.Util
open Testing

let ok_to_create_db (dbname : string) =
  let open Async.Std in
  let path = dbname ^ "/" ^ dbname ^ ".json" in
  Sys.file_exists_exn dbname >>= fun b ->
    if b then return false
    else Unix.mkdir (~perm:0o777) dbname >>= fun () ->
      Writer.open_file path >>= fun t ->
        Writer.write t ""; Writer.close t >>= fun () ->
          return true

let to_string_list json =
  let open Yojson.Basic.Util in
  json
  |> to_list
  |> List.map to_string

let remove dirname =
  let open Async.Std in
  Sys.ls_dir dirname >>= fun l ->
    traverse l >>= fun () ->
      Unix.rmdir dirname

(* [create_full_table db tablename j] creates the table with
 * name tablename and contents as specified by the JSON value j.
 * The table is added to database db.
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message
 * val create_full_table: database -> string -> json -> result *)
let create_full_table (db: database) (tablename : string) json =
  match
    (try
      let table_name =                    (* table_name : string *)
        json
        |> member "tableName"
        |> to_string in
      let column_names =                  (* column_names : string list *)
        json
        |> member "columnNames"
        |> to_string_list in
      let columns =                       (* columns : string list list *)
        json
        |> member "columns"
        |> to_list
        |> List.map to_string_list in
      Some (table_name, column_names, columns)
    with
    | _ -> None)
  with
  | None -> Failure ("Incorrectly formatted json file for table " ^
    tablename ^ ". It should have the following 3 fields: tableName, " ^
    "columnNames, and columns.\n")
  | Some (table_name, column_names, columns) ->
    if List.length columns <> List.length column_names
    then Failure ("Incorrectly formated json file for table " ^ tablename ^
    ". columnNames and columns should be of the same length.\n")
    else create_whole_table db table_name column_names columns

let load_table (db : database) (tablename : string) =
  let dbname = db.name in
  let path = dbname ^ "/" ^ tablename ^ ".json" in
  if not (Sys.file_exists path) then Failure ("Cannot find file " ^
    tablename ^ ".json in directory " ^ dbname ^ ".\n")
  else match
    (try
      Some (Yojson.Basic.from_file path)
     with
     | _ -> None)
  with
  | Some table -> create_full_table db tablename table
  | None -> Failure ("Cannot parse file " ^ tablename ^
    ".json in directory " ^ dbname ^ ".\n")

let rec load_tables tables db =
  match tables with
  | [] -> Success db
  | hd :: tl -> match load_table db hd with
                | Success db' -> load_tables tl db'
                | s -> s

let no_failure f = function
  | Failure s -> Failure s
  | Success db -> f db
  | _ -> failwith "This should never pattern match"

(* [create_db dbname j] creates the database with name
 * dbname and contents as specified by the JSON value j
 * Returns Success if the operation was successfully completed
 * Otherwise returns Failure some_error_message
 * val create_db: string -> json -> result *)
let create_db (dbname: string) json =
  match
    (try
      let dbname =
        json
        |> member "dbName"
        |> to_string in
      let tables =
        json
        |> member "tables"
        |> to_string_list in
      Some (dbname, tables)
    with
    | _ -> None)
  with
  | None -> Failure ("Incorrectly formatted json file for database " ^
    dbname ^ ". It should have the following 2 fields: dbname, tables.\n")
  | Some (dbname, tables) -> let x1 = create_database dbname in
                             no_failure (load_tables tables) x1

let load_db (dbname : string) =
  let path = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
  if not (Sys.file_exists path) then Failure ("Cannot find folder " ^ dbname
      ^ " in current directory.\n")
  else match (try Some (Yojson.Basic.from_file path) with _ -> None) with
       | Some json -> create_db dbname json
       | None -> Failure ("Cannot parse file " ^ dbname ^ ".json in directory "
         ^ dbname ^ ".\n")

let delete_table (p1 : string) (tablename : string) =
  let path = p1 ^ "/" ^ tablename ^ ".json" in
  if Sys.file_exists path then Sys.remove path else ()

let drop_db (dbname : string) =
  let open Async.Std in
  let path = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
  let db = {name = ""; data = Tst.create (); updated = Ivar.create ()} in
  Sys.file_exists_exn path >>= fun b ->
    if b then remove dbname >>= fun () ->
      return (Success db)
    else return (Failure ("Database " ^ dbname ^ "does not exist.\n"))

(******************************************************************************)
(********************************UNIT TESTS************************************)
(******************************************************************************)

TEST_MODULE "to_string" = struct
  let l1 = `List [`String "s1"; `String "s2"; `String "s3"]
  let l2 = `List []
  TEST "empty_list" = (to_string_list l2) = []
  TEST "full_list" = (to_string_list l1) = ["s1"; "s2"; "s3"]
end

TEST_MODULE "create_db" = struct
  open Async.Std
  (* improper fields in db file *)
  let _ = Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("db_name", `String "RJtest");
          ("tab", `List [`String "t1"; `String "t2"])]
  TEST = (create_db "RJtest" j) = Failure ("Incorrectly formatted json file " ^
    "for database RJtest. It should have the following 2 fields: dbname, " ^
    "tables.\n")

  (* missing fields in db file *)
  let _ = Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("dbName", `String "RJtest")]
  TEST = create_db "RJtest" j = Failure ("Incorrectly formatted json file " ^
    "for database RJtest. It should have the following 2 fields: dbname, " ^
    "tables.\n")

  (* no table files *)
  let _ = Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  TEST = create_db "RJtest" j = Failure ("Cannot find file t1.json in " ^
    "directory RJtest.\n")

  (* t1 but no t2 file *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"];
       `List [`String "Blue"; `String "Green"; `String "Purple"]])]
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = create_db "RJtest" j = Failure ("Cannot find file t2.json in " ^
    "directory RJtest.\n")

  (* improper table file *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  let d = Writer.open_file "RJtest/t1.json" >>= fun t ->
  Writer.write t "shksjdksjdkajk"; Writer.close t
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> d)
  TEST = create_db "RJtest" j = Failure ("Cannot parse file t1.json in " ^
    "directory RJtest.\n")

  (* success *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"];
       `List [`String "Blue"; `String "Green"; `String "Purple"]])]
  let t2 = `Assoc [("tableName", `String "t3"); ("columnNames",
    `List [`String "hi"]);
   ("columns",
    `List
      [`List [`String "cool"; `String "as"; `String "a"; `String "cucumber"]])]
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  let () = Yojson.Basic.to_file "RJtest/t2.json" t2
  TEST = test_success (create_db "RJtest" j)
end

TEST_MODULE "create_full_table" = struct
  open Async.Std
  (* improper field names for table json *)
  let db = { name = ""; data = Tst.create (); updated = Ivar.create ()}

  let t1 = `Assoc [("name", `String "t1");
   ("colnames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"];
       `List [`String "Blue"; `String "Green"; `String "Purple"]])]
  TEST = create_full_table db "t1" t1 = Failure ("Incorrectly formatted json " ^
    "file for table t1. It should have the following 3 fields: tableName, " ^
    "columnNames, and columns.\n")

  (* column and columnNames do not match up *)
  let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"]])]
  TEST = create_full_table db "t1" t1 = Failure ("Incorrectly formated json " ^
    "file for table t1. columnNames and columns should be of the same length.\n")

  (* success *)
  let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"];
       `List [`String "Blue"; `String "Green"; `String "Purple"]])]
  TEST = test_success (create_full_table db "t1" t1)
end