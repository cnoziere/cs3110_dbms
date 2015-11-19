(**
 * key is the primary key, unique in each table.
 *)
type key = int

(**
 * value is the value type stored in the database.
 *)
type value = string

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

(**
 * op is the type of operators used in WHERE clauses.
 *)
type op = Eq | NotEq | Gt | Lt | GtEq | LtEq | Between | Like | In

(**
 * results signal whether a database operation was successful or failed.
 * Failures contain a user-facing message to display.
 *)
type result = Success | Failure of string
