open Types
open Async.Std


(**
 * A column is represented by a BST with integer keys (the row key)
 * and string data (table values).
 *)
type column = string Bst.tree ref

(**
 * A table is represented by a TST with string keys (the names of each column)
 * contains columns
 *)
type table = column Tst.tree ref


(*
Reminder of type result
type result = Success | Failure of string | Column of value list
      | PFailure of string | PMessage of string
      | OpColumn of value list list
*)

let db = ref Tst.create ()


let update = failwith "TODO"

let updated = failwith "TODO"

let create_table = (table_name: string) (col_names: string list): result =
    if col_names = [] then Failure ("No column names are provided to " ^
        "initialize table") else
    let new_table = ref Tst.create () in
    let key_column = ref Bst.create () in
    (* Column of primary keys is named the empty string *)
    let (_, new_table') = insert "" key_column !new_table in
    new_table := new_table';
    let rec add_cols = function
        | [] ->
            let (is_duplicate, db') = Tst.insert table_name !new_table !db in
            if is_duplicate then
                Failure "Table name already exists in database"
            else
                db := db';
                Success
        | h::t ->
            let new_column = ref Bst.create () in
            let (is_duplicate, new_table') = Tst.insert "" new_column !new_table in
            if is_duplicate then
                Failure "Duplicate column name used to initialize table"
            else
                new_table := new_table';
                helper t in
    helper add_cols


let drop_table = failwith "TODO"

let add_row = failwith "TODO"

let get_row = failwith "TODO"

let delete_row = failwith "TODO"

let update_value = failwith "TODO"

let get_column_names: string -> string list

let get_column_vals: string -> string -> (value -> bool) -> result




let get_value_table = failwith "TODO"

let get_value_col = failwith "TODO"

let delete_col = failwith "TODO"

let get_table = failwith "TODO"
