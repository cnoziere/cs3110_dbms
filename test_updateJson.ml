(******************************************************************************)
(*************************Unit Tests for UpdateJson****************************)
(******************************************************************************)

(* See updateJson.ml for tests of functions not included in the UpdateJson
signature *)

open Yojson.Basic
open UpdateJson
open ReadJson
open Async.Std
open Testing
open Types

let j = `Assoc [("dbName", `String "RJtest");
        ("tables", `List [`String "t1"])]

let t1 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"]);
   ("columns",
    `List
      [`List [`String "Asta"; `String "Amanda"]])]

let t1' = `Assoc [("tableName", `String "t1"); ("columnNames", `List [`String "Name"]);
   ("columns",
    `List [`List [`String "Asta"; `String "Amanda"; `String "Constance"]])]

let t2 = `Assoc [("tableName", `String "t1");
   ("columnNames", `List [`String "Name"; `String "Age"; `String "Color"]);
   ("columns", `List [`List []; `List []; `List []])]

let j' = `Assoc [("dbName", `String "RJtest");
        ("tables", `List [`String "t1"; `String "t2"])]

TEST_MODULE "watch_for_update" = struct
  (* table modified *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = match ReadJson.load_db "RJtest" with
         | Success db -> let res = InternalRep.add_row db "t1" ["Name"] ["Constance"] in
                         ignore(watch_for_update_testing db);
                         (match res with
                          | Success db' -> let b1 = test_async_eq (Sys.file_exists_exn "RJtest/t1.json") true in
                                           let b2 = db <> db' in
                                           let b3 = (Yojson.Basic.from_file "RJtest/t1.json") = t1' in
                                           b1 && b2 && b3
                          | _ -> false)
        | _ -> false

  (* table added *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = match ReadJson.load_db "RJtest" with
         | Success db -> let res = Operation.create_table db "t2" ["Name"; "Age"; "Color"] in
                         ignore(watch_for_update_testing db);
                         (match res with
                          | Success db' -> let b1 = test_async_eq (Sys.file_exists_exn "RJtest/t2.json") true in
                                           let b2 = db <> db' in
                                           (* let b3 = (Yojson.Basic.from_file "RJtest/t2.json") = t2 in
                                           let b4 = (Yojson.Basic.from_file "RJtest/RJtest.json") = j' in *)
                                           b1 && b2 (* && b3 && b4 *)
                          | _ -> false)
        | _ -> false

  (* table removed *)
(*  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1
  TEST = match ReadJson.load_db "RJtest" with
         | Success db -> ignore(watch_for_update_testing db);
                         let res = Operation.drop_table db "t2" in
                         (match res with
                          | Success db' -> let b1 = test_async_eq (Sys.file_exists_exn "RJtest/t2.json") false in
                                           let b2 = db <> db' in
                                           let b3 = (Yojson.Basic.from_file "RJtest/RJtest.json") = j in
                                           b1 && b2 && b3
                          | _ -> false)
         | _ -> false *)

  let () = ignore(exit 0)

end

let _ = Scheduler.go ()