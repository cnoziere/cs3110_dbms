(******************************************************************************)
(*************************Unit Tests for ReadJson******************************)
(******************************************************************************)

(* See readJson.ml for tests of functions not included in the ReadJson
signature *)
open Yojson.Basic
open Testing
open ReadJson
open Async.Std
open Types

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