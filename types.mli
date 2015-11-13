(**
 * key is the primary key, unique in each table.
 *)
type key = int

(**
 * value is the value type stored in the database.
 *)
type value = string

(**
 * column is a list of pairs, where name is the row name, and value is the
 * value stored at that row key and column in the table.
 *)
type column = (key * value) list

(**
 * table is a list of pairs containing a column name and a column.
 *)
type table = (string * column) list

(**
 * database is a list of pairs containing a table name and a table.
 *)
type database = (string * table) list

(**
 * op is the type of operators used in WHERE clauses.
 *)
type op = Eq | NotEq | Gt | Lt | GtEq | LtEq | Between | Like | In

(**
 * results signal whether a database operation was successful or failed.
 * Failures contain a user-facing message to display.
 *)
type result = Success | Failure of string
