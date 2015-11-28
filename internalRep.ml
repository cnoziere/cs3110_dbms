open Types
open Async.Std


(**
 * A column is represented by a BST with integer keys (the row key)
 * and string data (table values).
 *)
type column = string Bst.tree ref

(**
 * A table is represented by a TST with string keys (the names of each column)
 * containing columns
 *)
type table = column Tst.tree ref

(**
 * A database is represented by a TST with string keys (the names of each table)
 * containing tables
 * The mutable updated field contains an Ivar that is filled when a change is
 * made, and is immediately replaced by an unfilled Ivar
 *)
type database =
{
    (* name: string, in case of multiple databases *)
    mutable data: table Tst.tree;
    mutable updated: unit Ivar.t;
}

(*
Reminder of type result
type result = Success | Failure of string | Column of value list
      | PFailure of string | PMessage of string
      | OpColumn of value list list
*)

let db: database =
{
    data = Tst.create ();
    updated = Ivar.create ();
}

(**
 * [update] updates the data field with a new tree,
 * fills the Ivar in the database (the deferred read in [updated]
 * becomes determined), and then replaces the updated field with an empty Ivar
 *)
let update (data: table Tst.tree) =
    db.data <- data;
    Ivar.fill db.updated;
    db.updated <- Ivar.create ()

let updated = Ivar.read db.updated

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
