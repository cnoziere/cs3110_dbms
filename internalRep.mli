(**
 * This module contains the internal representation of data in a database and
 * methods to read and modify the stored data
 *)

(**
 * database is a reference to a tree that stores:
 *   t_key: tree keys are table names, of type [string]
 *   t_value: tree values are of type [table]
 *)
type database = table tree ref

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

(**
 * result signals whether a database operation was successful or failed.
 * Failures contain an error message
 * Successes contain the value resulting from the operation
 *)
type 'a result = Success of 'a | Failure of string

(**
 * Create table, given the table name and a list of the column names
 * Return table with empty columns
 *)
val create_table: (table_name: string) -> (column_names: string list) -> table

(**
 * Add new row to a table, given the table, a list of the column names and
 * a list of the respective values to populate the row
 * Values not provided are assigned empty strings
 * Returns Failure if the row is an exact duplicate, otherwise returns Success
 * Returns Failure if table or columns do not exist
 *)
val add_row: (table_name: string) -> (column_names: string list) -> (value list) -> unit result

(**
 * Given the table, a list of the column names and a list of the respective
 * values, return the keys for all the rows for which the
 * column values match
 * If the provided columns and values are empty, return all keys
 * Returns Failure if the columns list and val list do not have the same length
 * Returns Failure if table or columns do not exist
 *)
val get_row: (table_name: string) -> (column_names: string list) -> (value list)
    -> key list result

(**
 * Given the table, the column name, and key, return the value in table at
 * key, col_name
 * Returns Failure if table, key or column does not exist
 *)
val get_value: (table_name: string) -> (col_name: string) -> key -> value result

(**
 * Given the table, the column name, key, and a new value, update the value
 * in table_name at key, col_name
 * Returns Failure if table, key or column does not exist
 *)
val update_value: (table_name: string) -> (col_name: string) -> key
    -> (updated: val) -> unit result

(**
 * Given the table and key, delete row associated with the key in the
 * given table
 * Returns Failure if table, key or column does not exist
 *)
val delete_row: (table_name: string) -> key -> unit result

(**
 * Given the table, return string list of column names
 * Returns Failure if table does not exist
 *)
val get_columns: (table_name: string) -> string list result
