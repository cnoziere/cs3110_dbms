open Yojson.Basic
open Async.Std
open Types

(* This function implements the observer pattern from Rec 18
 * [watch_for_update db] performs one of the following:
 * 1) if a table from db has been modified, it writes the new table to file
 * 2) if a table has been added, it writes the updated database and new table
   to file
 * 3) if a table has been removed, it writes the updated database and removes
 * the table from file *)
val watch_for_update: database -> unit Deferred.t