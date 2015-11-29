open Types
open Async.Std

(**
 * This module contains the internal representation of data in a database and
 * methods to read and modify the stored data
 *)

(**
 * returns a deferred that becomes determined once the database
 * has been modified
 *)
val updated : unit -> unit Deferred.t

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
 * Values for columns not provided are assigned empty strings ("NULL")
 * Return result of Success or Failure
 * Precondition: string list and value list must be of the same length
 *)
val add_row: string -> string list -> value list -> result


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
 * Given the table name, return result of ColName, a string list of column names
 *)
val get_column_names: string -> result

(**
 * Given the table name, the column name, and a function (value -> bool), return
 * values in the column that satisfy the function (result Column of value list)
 * To return all values in a column, input a function that always returns true
 * Returns result of Failure if table or column names do not exist
 *)
val get_column_vals: string -> string -> (value -> bool) -> result

(**
 * Given the table name, the column name, and a function (value -> bool), return
 * the keys for all values that satisfy the function
 *)
val get_row: string -> string -> (value -> bool) -> result

(*

NOT NECESSARY?

(**
 * Given the table, the column name, and key, return the value in table at
 * key, col_name
 *)
val get_value_table: table -> string -> key -> value

(**
 * Given the table and column name, delete column
 *)
val delete_col: table -> string -> unit

*)
