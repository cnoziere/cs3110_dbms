(******************************************************************************)
(*************************Unit Tests for UpdateJson****************************)
(******************************************************************************)

(* See updateJson.ml for tests of functions not included in the UpdateJson
signature *)

open Yojson.Basic
open UpdateJson
open Async.Std
open Testing
open Types

(* TEST_MODULE "watch_for_update" = struct
  (* table modified *)
  let _ = Thread_safe.block_on_async (fun () -> create_dir ())
  let () = Yojson.Basic.to_file "RJtest/RJtest.json" j
  let () = Yojson.Basic.to_file "RJtest/t1.json" t1'
  TEST = match ReadJson.load_db "RJtest" with
        | Success db -> let res = InternalRep.add_row db "RJtest" ["Name"] ["Constance"] in
                        (match res with
                        | Success db' -> let b1 = test_async_eq (Sys.file_exists_exn "RJtest/t1.json") true in
                                         (* let b2 = db <> db' in
                                         let b3 = (Yojson.Basic.from_file "RJtest/t1.json") = r in *)
                                         b1
                        | _ -> false)
        | _ -> false

  (* table added *)

  (* table removed *)

end *)