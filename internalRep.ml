open Types
open Async.Std

(**
 * A column is represented by a BST with integer keys (the row key)
 * and string data (table values).
 *)
type column = string Bst.tree

(**
 * A table is represented by a TST with string keys (the names of each column)
 * contains columns
 *)
type table = column Tst.tree

(**
 * A database is represented by a TST with string keys (the names of each table)
 * contains tables
 *)
type database = table Tst.tree

(*
Reminder of type result
type result = Success | Failure of string | Column of value list
      | PFailure of string | PMessage of string
      | OpColumn of value list list
*)


let update = failwith "TODO"

let updated = failwith "TODO"

let create_table = failwith "TODO"

let drop_table = failwith "TODO"

let add_row = failwith "TODO"

let get_row = failwith "TODO"

let delete_row = failwith "TODO"

let update_value = failwith "TODO"

let get_column = failwith "TODO"

let get_value_table = failwith "TODO"

let get_value_col = failwith "TODO"

let delete_col = failwith "TODO"

let get_column_list = failwith "TODO"

let get_table = failwith "TODO"
