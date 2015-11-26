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
 * Updates a table, given the table name, a list of (column name, value) pairs
 * to identify the row, and the new data as a list of (column name, value
 * pairs. Returns a type result.
 *)
val update : string -> (string * value) list -> (string * value) list -> result

(**
 * Finds values that match the select-from command, given:
 *   column names (select)
 *   table name (from)
 * Returns a table of the result set.
 *)
val select_from : string list -> string -> table

(**
 * Finds values that match the select-from-where commands, given:
 *   list of column names (select)
 *   table name (from)
 *   triple of column name, operator, value (where) that results must match
 * Returns a table of the result set.
 *)
val select_from_where : string list -> string -> (string * op * value) -> table
