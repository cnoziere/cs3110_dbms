open Tree
open Async.Std

(**
 * key is the primary key, unique in each table.
 *)
type key = int

(**
 * value is the value type stored in the database.
 *)
type value = string

(**
 * op is the type of operators used in WHERE clauses.
 *)
type op = Eq | NotEq | Gt | Lt | GtEq | LtEq

(**
 * results signal whether a database operation was successful or failed.
 * Failures contain a user-facing message to display.
 * P types indicate whether parsing succeeded or failed.
 *)
type result = Success | Failure of string | Column of value list
              | PFailure of string | PMessage of string
              | OpColumn of value list list
