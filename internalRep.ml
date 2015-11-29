open Types
open Async.Std


(**
 * A column is represented by a BST with integer keys (the row key)
 * and string data (table values).
 *)
type column =
{
    mutable data: value Bst.tree;
    mutable length: int;
}

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
    Ivar.fill db.updated ();
    db.updated <- Ivar.create ()

let updated () = Ivar.read (db.updated)

let create_table (table_name: string) (col_names: string list): result =
    if col_names = [] then Failure ("No column names are provided to " ^
        "initialize table") else
    let (new_table: table) = ref (Tst.create ()) in
    let (key_column: column) = {data = Bst.create (); length = 0} in
    (* Column of primary keys is named the empty string *)
    let (_, new_table') = Tst.insert "" key_column !new_table in
    new_table := new_table';
    let rec add_cols = function
        | [] ->
            let (is_duplicate, new_db) = Tst.insert table_name new_table db.data in
            if is_duplicate then
                Failure "Table name already exists in database"
            else
                (update new_db;
                Success)
        | h::t ->
            let (new_column: column) = {data = Bst.create (); length = 0} in
            let (is_duplicate, new_table') =
                Tst.insert h new_column !new_table in
            if is_duplicate then
                Failure "Duplicate column name used to initialize table"
            else
                (new_table := new_table';
                add_cols t) in
    add_cols col_names

let drop_table (dropped: string): result =
    let (success, new_db) = Tst.remove dropped db.data in
    if success then
        (update new_db;
        Success)
    else
        Failure (dropped ^ " is not a table in the database")

(**
 * Add new row to a table, given the table name, a list of the column names,
 * and a list of the respective values to populate the row
 * Values not provided are assigned empty strings
 * Return result of Success or Failure
 *)

(* Returns true if all items in sublst are a member of assoc_lst *)
let rec all_mem (sublst: 'a list) (assoc_lst: ('a * 'b) list): bool =
    match sublst with
    | [] -> true
    | h::t ->
        try
            ignore(List.assoc h assoc_lst);
            true && (all_mem t assoc_lst)
        with
        | Not_found -> false


let add_row (table_name: string) (cols_to_change: string list)
    (vals: value list): result =
    if List.length cols_to_change <> List.length vals then
        Failure "Number of columns and values to be added do not match"
    else match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some table_to_change ->
        (* Cols is the associative list of col names and columns *)
        let cols = Tst.list_tst (!table_to_change) in
        if cols = [] then
            Failure "Table has no columns"
        else if all_mem cols_to_change cols then
            let rec add_cols = function
            | [] ->
                (update db.data;
                Success)
            | (name, curr_col)::t ->
                (* [find_cols]: find the value to be inserted into curr_col *)
                let rec find_cols names vs =
                    match names, vs with
                    | [],[] -> ""
                    | h1::t1, h2::t2 ->
                        if h1 = name then h2
                        else find_cols t1 t2
                    | _,_ -> "" in (* case not possible *)
                let (updated, new_col) =
                    Bst.insert curr_col.length
                    (find_cols cols_to_change vals) curr_col.data in
                if updated then Failure "Row key already exists"
                else
                    (curr_col.length <- curr_col.length + 1;
                    curr_col.data <- new_col;
                    add_cols t) in
            add_cols cols
        else
            Failure ("Provided columns do not exist in the table " ^ table_name)


let delete_row (table_name: string) (key_to_delete: key): result =
    match Tst.get table_name db.data with
    | None -> Failure (table_name ^ " is not a table in the database")
    | Some table_to_change ->
        (* Cols is the associative list of col names and columns *)
        let cols = Tst.list_tst (!table_to_change) in
        if cols = [] then
            Failure "Table has no columns"
        else
            (* [remove_key] removes the key_to_delete from all columns in cols *)
            let rec remove_key = function
            | [] ->
                (update db.data;
                Success)
            | (_, curr_col)::t ->
                let (x: column) = curr_col in
                let (removed, new_col) = Bst.remove key_to_delete (x.data) in
                if removed then
                    (curr_col.length <- curr_col.length - 1;
                    curr_col.data <- new_col;
                    remove_key t)
                else Failure "Key does not exist" in
            remove_key cols


(*

let update_value = failwith "TODO"

let get_column_names: string -> string list

let get_column_vals: string -> string -> (value -> bool) -> result



let get_row = failwith "TODO"

let get_value_table = failwith "TODO"

let get_value_col = failwith "TODO"

let delete_col = failwith "TODO"

let get_table = failwith "TODO"

*)
