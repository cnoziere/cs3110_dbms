(**
 * This module contains the internal representation of data in a database and
 * methods to read and modify the stored data
 *)

(**
 * database is a tree that stores:
 *   t_key: tree keys are table names, of type [string]
 *   t_value: tree values are of type [table]
 *)
type database = {tables: (table_name: string, table) list; updated : t Ivar.t}

(**
 * table is a reference to a tree that stores:
 *   t_key: tree keys are column names, of type [string]
 *   t_value: tree values are of type [column]
 *)
type table = column tree ref

(**
 * column is a reference to a tree that stores:
 *   t_key: tree keys are of type [value]
 *   t_value: tree values are of type [key]
 * where [value] is the table value at the column and the row [key]
 *)
type column = key tree ref

(* update creates a new database with an empty Ivar and fills the
updated field of the current database with the new database *)
val update : database -> unit

(* returns a deferred that becomes determined once the database
has been modified *)
val updated : database -> 'a Deferred.t

(**
 * Create table, given the table name and a list of the column names
 * Return table with empty columns
 *)
val create_table: (table_name: string) -> (column_names: string list) -> table

(**
 * Add new row to a table, given the table, a list of the column names and
 * a list of the respective values to populate the row
 * Values not provided are assigned empty strings
 *)
val add_row: table -> (column_names: string list)
    -> (value list) -> unit

(**
 * Given the table, a list of the column names and a list of the respective
 * values, return the keys for all the rows for which the
 * column values match
 * If the provided columns and values are empty, return all keys
 *)
val get_row: table -> (column_names: string list) -> (value list)
    -> key list

(**
 * Given the table, the column name, and key, return the value in table at
 * key, col_name
 *)
val get_value_table: table -> (col_name: string) -> key -> value

(**
 * Given the column and key, return the value at key, col_name
 *)
val get_value_col: column -> key -> value

(**
 * Given the table, the column name, key, and a new value, update the value
 * in table_name at key, col_name
 *)
val update_value: table -> (col_name: string) -> key
    -> (updated: val) -> unit

(**
 * Given the table and key, delete row associated with the key in the
 * given table
 *)
val delete_row: table -> key -> unit

(**
 * Given the table and column name, delete column
 *)
val delete_col: table -> (col_name: string) -> unit

(**
 * Given the table, return string list of column names
 *)
val get_column_list: table -> string list

(**
 * Given the table and column name, return column
 *)
val get_column: table -> (col_name: string) -> column

(**
 * Given the table name, return table in the database
 *)
val get_column: (table_name: string) -> table
