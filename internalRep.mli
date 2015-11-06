(* This module contains the internal representation of data in a database and
 * methods to read and modify the stored data *)

(* primary key, unique in each table *)
type key = string

(* value type, all values are representated as strings *)
type val = string

(* column: list of (row_name, val) pairs, where val is the value
 * stored at row_name, column in a table *)
type column = (row_name: string, val) list

(* table: list of (col_name, column) pairs *)
type table = (col_name: string, column) list

(* database: list of (table_name, table) pairs *)
type database = (table_name: string, table) list

(* create table with empty columns *)
val create_table: (table_name: string) -> (columns: string list)
    -> Operation.result

(* add new row to a table and populate the given values
 * values not provided are assigned empty strings *)
val add_row: (table_name: string) -> (columns: string list)
    -> (val list) -> Operation.result

(* Returns list of keys for all rows matching the column values
 * The columns list and val list must have the same length
 * If the provided columns and values are empty, return all keys *)
val get_row: (table_name: string) -> (columns: string list)
    -> (val list) -> key list

(* Returns value in table_name at row_name, column *)
val get_value: (table_name: string) -> column -> (row_name: string)
    -> Operation.result

(* Updates value in table_name at row_name, column *)
val update_value: (table_name: string) -> column -> (row_name: string)
    -> (updated: val) -> Operation.result

(* Deletes row associated with the key in the given table *)
val delete_row: (table_name: string) -> key -> Operation.result

(* Return string list of column names *)
val get_columns: (table_name: string) -> string list
