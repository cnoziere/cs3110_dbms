(******************************************************************************)
(** Unit tests for readJson Implementation ************************************)
(******************************************************************************)

open Yojson.Basic
open ReadJson
open Async.Std

let test_async_eq (d : 'a Async.Std.Deferred.t) (v : 'a) : bool =
  Async.Std.Thread_safe.block_on_async (fun () -> d) = Core.Std.Result.Ok v

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
  let _ = Async.Std.Thread_safe.block_on_async (fun () -> remove_dir ())
  TEST = test_async_eq (ok_to_create_db "RJtest") true

  let _ = Async.Std.Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" `Null
  TEST = test_async_eq (ok_to_create_db "RJtest") false

  (* let d2 = Sys.remove "RJtest1/RJtest1.json" >>= fun () ->
  Unix.rmdir "RJtest1"
  let _ = Thread_safe.block_on_async (fun () -> d2)
  TEST = test_async_eq (ok_to_create_db "RJtest1") false *)

(* let d3 = Sys.remove "RJtest1/RJtest1.json"
  let _ = Thread_safe.block_on_async (fun () -> d3)
  TEST = test_async_eq (ok_to_create_db "RJtest1") true
end *)
end

TEST_MODULE "load_dn" = struct
  (* let d = Sys.file_exists_exn "RJtest1"
  let d1 = (d >>= fun b ->
    (if b then return ()
    else Unix.mkdir "RJtest1") >>= fun () ->
    Writer.open_file "RJtest1/RJtest1.json" >>=
    fun t -> Writer.write t ""; Writer.close t)
  let _ = Thread_safe.block_on_async (fun () -> d1)
  TEST = test_async_eq (ok_to_create_db "RJtest1") false *)
end