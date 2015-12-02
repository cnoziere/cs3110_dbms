open Types

(**
 * Creates a database, given the database name. Returns Success or Failure.
 *)
val create_database : string -> result

(**
 * Creates a table, given the table name and list of column names.
 * Returns a type result.
 *)
val create_table : database -> string -> string list -> result

(**
 * Deletes a table, given the table name. Returns a type result.
 *)
val drop_table : database -> string -> result

(**
 * Adds a row to a table, given the table name, list of column names,
 * and list of values. Returns a type result.
 *)
val add_row : database -> string -> string list -> value list -> result

(**
 * Deletes a row from a table, given the table name, and None or a
 * (column name, value) pair to identify the row. Returns a type result.
 *)
val delete_row : database -> string -> (string * op * value) option -> result

(**
 * Updates a table, given the table name, the new data as a list of
 * column names and a list of values, and None or a (column name, value) pair
 * to identify the row. Returns a type result.
 *)
val update : database -> string -> string list -> value list ->
             (string * op * value) option -> result

(**
 * Finds values that match the select-from-where commands. Expects:
 *   - table name
 *   - None if no columns specified, or list of column names
 *   - None if no where conditions, or column name, operator, value triple
 * Returns a result Failure if any selection fails. Otherwise, if all succeed,
 * returns a result OColumn containing the value lists for each selection.
 *)
val select : database -> string -> string list option ->
             (string * op * value) option -> result

(**
 * Given a table name, returns result Failure if table doesn't exist, or
 * OpColumn of all columns.
 *)
val get_table : database -> string -> result

(**
 * Helper functions. [check_failures] is for [delete_row] and [update].
 * [filter_columns] used by [select_from] and [select_from_where].
 *)
val concat_failures : result list -> string -> result
val check_failures : result list -> result
val concat_columns : result list -> value list list -> result
val filter_columns: result list -> result
