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

  (* directory and file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" `Null
  TEST = test_async_eq (ok_to_create_db "RJtest") false

  (* directory but no file *)
  let d2 = Sys.remove "RJtest/RJtest.json"
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> d2)
  TEST = test_async_eq (ok_to_create_db "RJtest") false

  (* test that the files are actually there!! *)

end

TEST_MODULE "create_db" = struct
  (* no table files *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("dbName", `String "RJtest");
          ("tables", `List [`String "t1"; `String "t2"])]
  TEST = create_db "RJtest" j = Failure ("Cannot find table t1 in directory RJtest.\n")

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
  TEST = create_db "RJtest" j = Failure ("Cannot find table t2 in directory RJtest.\n")

  (* improper db file *)
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  let j = `Assoc [("dbName", `String "RJtest")]
  TEST = create_db "RJtest" j = Failure ("Cannot parse the following file: ./RJtest/RJtest.json\n")

  (* improper table file *)
  (* let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
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
  TEST = create_db "RJtest" j = Failure ("Cannot parse table t1 in directory RJtest.\n") *)

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
  (* improper table file 1 *)
    (* Failure ("Cannot parse the json file for table " ^ tablename ^ "\n") *)

  (* improper table file 2 - List.length columns <> List.length column_names*)
      (* Failure ("Incorrectly formated json file for table " ^ tablename ^
      ". columnNames and columns should be of the same length.\n") *)

  (* success *)
end

TEST_MODULE "load_db" = struct
  (* no db file *)
    (* Failure ("Cannot find database " ^ dbname ^ " in current directory.\n") *)

  (* improper db file *)
    (* Failure ("Cannot parse the following file: " ^ path ^ "\n") *)

  (* success *)
end

TEST_MODULE "drop_db" = struct
  (* db folder doesn't exists *)
    (* Failure ("Database " ^ db.name ^ "does not exist.\n") *)

  (* db folder and main file exist *)
    (* check that db folder and all internal files deleted *)
    (* success *)
end