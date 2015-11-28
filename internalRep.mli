open Types
open Async.Std

(**
 * This module contains the internal representation of data in a database and
 * methods to read and modify the stored data
 *)

(**
 * update creates a new database with an empty Ivar and fills the
 * updated field of the current database with the new database
 *)
val update : database -> unit

(**
 * returns a deferred that becomes determined once the database
 * has been modified
 *)
val updated : database -> 'a Deferred.t

(**
 * Create table, given the table name and a list of the column names
 * Return result of Success or Failure
 *)
val create_table: string -> string list -> result

(**
 * Drop table, given the table name
 * Return result of Success or Failure
 *)
val drop_table: string -> result

(**
 * Add new row to a table, given the table name, a list of the column names,
 * and a list of the respective values to populate the row
 * Values not provided are assigned empty strings
 * Return result of Success or Failure
 *)
val add_row: string -> string list -> value list -> result

(**
 * Given the table name, a list of the column names, and a list of the
 * corresponding values, return the keys for all the rows for which the
 * column values match
 * If the provided columns and values are empty, return all keys
 *)
val get_row: string -> string list -> value list -> key list

(**
 * Given the table name and key, delete row associated with the key in the
 * given table
 * Return result of Success or Failure
 *)
val delete_row: string -> key -> result

(**
 * Given the table name, the column name, key, and a new value, update the
 * value in table_name at key, col_name
 * Return result of Success or Failure
 *)
val update_value: string -> string -> key -> value -> result

(**
 * Given the table name, return string list of column names
 *)
val get_column_names: string -> string list

(**
 * Given the table name, the column name, and a function (value -> bool), return
 * values in the column that satisfy the function (result Column of value list)
 * To return all values in a column, input a function that always returns true
 * Returns result of Failure if table or column names do not exist
 *)
val get_column_vals: string -> string -> (value -> bool) -> result



(* ATTTENTION: this is a temporary comment. Just to indicate some changes.
   - Everything above this comment is used by Operation (or some by JSON).
   - Below that are functions which aren't being used, so they could be
     removed from the .mli file entirely if no one needs them by the end. *)


(**
 * Given the table, the column name, and key, return the value in table at
 * key, col_name
 *)
val get_value_table: table -> string -> key -> value

(**
 * Given the column and key, return the value at key, col_name
 *)
val get_value_col: column -> key -> value

(**
 * Given the table and column name, delete column
 *)
val delete_col: table -> string -> unit

(**
 * Given the table name, return table in the database
 *)
val get_table: string -> table
