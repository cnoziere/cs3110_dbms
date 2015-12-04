(******************************************************************************)
(** Unit tests for readJson Implementation ************************************)
(******************************************************************************)

open Yojson.Basic
open ReadJson
open Async.Std
open Types

let test_async_eq (d : 'a Async.Std.Deferred.t) (v : 'a) : bool =
  Async.Std.Thread_safe.block_on_async (fun () -> d) = Core.Std.Result.Ok v

let test_success = function
  | Success _ -> true
  | _ -> false

let rec traverse = function
  | hd :: tl -> Sys.remove ("RJtest/" ^ hd) >>= fun () -> traverse tl
  | [] -> return ()

let create_dir () =
  Sys.file_exists_exn "RJtest" >>= fun b ->
  if b then Sys.ls_dir "RJtest" >>= fun l ->
  traverse l
  else Unix.mkdir "RJtest"

let remove_dir () =
  Sys.file_exists_exn "RJtest" >>= fun b ->
  if b then Sys.ls_dir "RJtest" >>= fun l ->
  traverse l >>= fun () ->
  Unix.rmdir "RJtest"
  else return ()

TEST_MODULE "ok_to_create_db" = struct
  (* no directory *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  TEST = test_async_eq (ok_to_create_db "RJtest") true
  TEST = test_async_eq (Sys.file_exists_exn "RJtest") true
  TEST = test_async_eq (Sys.file_exists_exn "RJtest/RJtest.json") true

  (* directory and file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" `Null
  TEST = test_async_eq (ok_to_create_db "RJtest") false

  (* directory but no file *)
  let d2 = Sys.remove "RJtest/RJtest.json"
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> d2)
  TEST = test_async_eq (ok_to_create_db "RJtest") false
end

TEST_MODULE "create_db" = struct
  (* improper fields in db file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("db_name", `String "RJtest");
          ("tab", `List [`String "t1"; `String "t2"])]
  (* TEST = (create_db "RJtest" j) = Failure ("Incorrectly formatted json file
  for database RJtest. It should have the following 2 fields: dbname, tables.\n") *)

  (* missing fields in db file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("dbName", `String "RJtest")]
  (* TEST = create_db "RJtest" j = Failure ("Incorrectly formatted json file
    for database RJtest. It should have the following 2 fields:
    dbname, tables.\n") *)

  (* no table files *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  TEST = create_db "RJtest" j = Failure ("Cannot find file t1.json in directory RJtest.\n")

  (* t1 but no t2 file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
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
  TEST = create_db "RJtest" j = Failure ("Cannot find file t2.json in directory RJtest.\n")

  (* improper table file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  let d = Writer.open_file "RJtest/t1.json" >>= fun t ->
  Writer.write t "shksjdksjdkajk"; Writer.close t
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> d)
  TEST = create_db "RJtest" j = Failure ("Cannot parse file t1.json in directory RJtest.\n")

  (* success *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"];
       `List [`String "Blue"; `String "Green"; `String "Purple"]])]
  let t2 = `Assoc [("tableName", `String "t3"); ("columnNames", `List [`String "hi"]);
   ("columns",
    `List
      [`List [`String "cool"; `String "as"; `String "a"; `String "cucumber"]])]
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  let () = Yojson.Basic.to_file "RJtest/t2.json" t2
  TEST = test_success (create_db "RJtest" j)
end

TEST_MODULE "create_full_table" = struct
  (* improper field names for table json *)
  let db = { name = ""; data = Tst.create (); updated = Ivar.create ()}

  let t1 = `Assoc [("name", `String "t1");
   ("colnames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"];
       `List [`String "Blue"; `String "Green"; `String "Purple"]])]
  (* TEST = create_full_table db "t1" t1 = Failure ("Incorrectly formatted json
    file for table t1. It should have the following 3 fields: tableName,
    columnNames, and columns.\n") *)

  (* column and columnNames do not match up *)
  let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"]])]
  (* TEST = create_full_table db "t1" t1 = Failure ("Incorrectly formated json
    file for table t1. columnNames and columns should be of the same length.\n") *)

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

TEST_MODULE "load_db" = struct
  (* no db folder *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  TEST = load_db "RJtest" = Failure ("Cannot find folder RJtest in current directory.\n")

  (* no db file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
  TEST = load_db "RJtest" = Failure ("Cannot find folder RJtest in current directory.\n")

  (* improperly formed db file *)
  let d = Writer.open_file "RJtest/RJtest.json" >>= fun t ->
  Writer.write t "shksjdksjdkajk"; Writer.close t
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> d)
  TEST = load_db "RJtest" = Failure ("Cannot parse file RJtest.json in directory RJtest.\n")

  (* success *)
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"])]
  let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns",
    `List
      [`List [`String "Constance"; `String "Asta"; `String "Amanda"];
       `List [`String "12"; `String "13"; `String "14"];
       `List [`String "Blue"; `String "Green"; `String "Purple"]])]
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = test_success (load_db "RJtest")
end


TEST_MODULE "drop_db" = struct
  (* db folder doesn't exists *)
    (* Failure ("Database " ^ db.name ^ "does not exist.\n") *)

  (* db folder and main file exist *)
    (* check that db folder and all internal files deleted *)
    (* success *)
end