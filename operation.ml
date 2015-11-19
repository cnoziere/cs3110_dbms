open Types

(**
 * Creates a table, given the table name and list of column names.
 * Returns a type result.
 *)
let create_table = failwith "TODO"

(**
 * Deletes a table, given the table name. Returns a type result.
 *)
let drop_table = failwith "TODO"

(**
 * Adds a row to a table, given the table name, list of column names,
 * and list of values. Returns a type result.
 *)
let add_row = failwith "TODO"

(**
 * Deletes a row from a table, given the table name, and a list of
 * (column name, value) pairs to identify the row. Returns a type result.
 *)
let delete_row = failwith "TODO"

(**
 * Updates a table, given the table name, a list of (column name, value) pairs
 * to identify the row, and the new data as a list of (column name, value
 * pairs. Returns a type result.
 *)
let update = failwith "TODO"

(**
 * Finds values that match the select-from command, given:
 *   column names (select)
 *   table name (from)
 * Returns a table of the result set.
 *)
let select_from = failwith "TODO"

(**
 * Finds values that match the select-from-where commands, given:
 *   list of column names (select)
 *   table name (from)
 *   triple of column name, operator, value (where) that results must match
 * Returns a table of the result set.
 *)
let select_from_where = failwith "TODO"
