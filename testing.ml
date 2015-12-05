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
