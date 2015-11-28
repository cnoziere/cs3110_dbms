open Types

(**
 * Creates a table, given the table name and list of column names.
 * Returns a type result.
 *)
val create_table : string -> string list -> result

(**
 * Deletes a table, given the table name. Returns a type result.
 *)
val drop_table : string -> result

(**
 * Adds a row to a table, given the table name, list of column names,
 * and list of values. Returns a type result.
 *)
val add_row : string -> string list -> value list -> result

(**
 * Deletes a row from a table, given the table name, and None or a
 * (column name, value) pair to identify the row. Returns a type result.
 *)
val delete_row : string -> (string * value) option -> result

(**
 * Updates a table, given the table name, the new data as a list of
 * column names and a list of values, and None or a (column name, value) pair
 * to identify the row. Returns a type result.
 *)
val update : string -> string list -> value list ->
             (string * value) option -> result

(**
 * Finds values that match the select-from command, given column names
 * list (select) and table name (from).
 * Passing in column names as None indicates that all column lists are expected.
 * Returns a result Failure if any selection fails. Otherwise, if all succeed,
 * returns a result OColumn containing the value lists for each selection.
 *)
val select_from : string list option -> string -> result

(**
 * Finds values that match the select-from-where commands, given column names
 * list (select), table name (from), and column name, operator, value (where)
 * that results must match.
 * Returns a result Failure if any selection fails. Otherwise, if all succeed,
 * returns a result OColumn containing the value lists for each selection.
 *)
val select_from_where : string list -> string -> string -> op -> value -> result
