
 (**
  * database is a tree that stores:
  *   t_key: tree keys are table names, of type [string]
  *   t_value: tree values are of type [table]
  *)
(* database: list of (table_name, table) pairs *)
type database = {tables: (table_name: string, table) list; updated : t Ivar.t}

(**
 * table is a tree that stores:
 *   t_key: tree keys are column names, of type [string]
 *   t_value: tree values are of type [column]
 *)
type table = (col_name: string, column) list

(**
 * column is a tree that stores:
 *   t_key: tree keys are of type [value]
 *   t_value: tree values are of type [key]
 * where [value] is the table value at the column and the row [key]
 *)
type column = key tree

(* update creates a new database with an empty Ivar and fills the
updated field of the current database with the new database *)
val update : database -> unit

(* returns a deferred that becomes determined once the database
has been modified *)
val updated : database -> 'a Deferred.t

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