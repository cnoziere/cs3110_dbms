(******************************************************************************)
(*************************Unit Tests for UpdateJson****************************)
(******************************************************************************)
open Testing
open Async.Std
open Yojson.Basic
open Types

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
         | Success db -> let _ = Thread_safe.block_on_async (fun () -> create_dir ()) in
                         let b1 = test_async_eq (Sys.file_exists_exn "RJtest/t1.json") false in
                         let _ = Thread_safe.block_on_async (fun () -> table_to_file db "t1") in
                         let b2 = test_async_eq (Sys.file_exists_exn "RJtest/t1.json") true in
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
         | Success db -> let _ = Thread_safe.block_on_async (fun () -> create_dir ()) in
                         let _ = Thread_safe.block_on_async (fun () -> Sys.remove "RJtest/RJtest.json") in
                         let b1 = test_async_eq (Sys.file_exists_exn "RJtest/RJtest.json") false in
                         let _ = Thread_safe.block_on_async (fun () -> database_to_file db) in
                         let b2 = test_async_eq (Sys.file_exists_exn "RJtest/RJtest.json") true in
                         let t' = Yojson.Basic.from_file "RJtest/RJtest.json" in
                         let b3 = t' = j in
                         b1 && b2 && b3
         | _ -> false
end

TEST_MODULE "watch_for_update" = struct
end