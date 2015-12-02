open Types
open Async.Std

(**
 * This module contains the internal representation of data in a database and
 * methods to read and modify the stored data
 *)

(**
 * Create a new database given a name
 *)
val create_database: string -> unit

(*
(**
 * returns a deferred that becomes determined once the database
 * has been modified
 *)
val updated: database -> database Deferred.t

(**
 * Get name of current database
 *)
val get_name: unit -> string

(**
 * Set name of current database
 *)
val set_name: string -> unit

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
 * Set names of all tables in current database
 *)
val get_table_names: unit -> string list

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
 * Given an empty column name "", [get_row] returns all keys
 *)
val get_row: string -> string -> (value -> bool) -> result

(**
 * Given the table name, the column name, and a list of keys, return the values
 * in the column corresponding to the keys
 *)
val get_values: string -> string -> key list -> result

(**
 * Create new table and populate with values
 * Input: table name, list of column names, list of values in each column
 * Precondition: list of column names must be the same length as list of value lists
val create_whole_table: string -> string list -> value list list -> result
 *)

*)
