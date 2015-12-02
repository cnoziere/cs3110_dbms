open Async.Std
open Yojson.Basic

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
 * A column is represented by a BST with integer keys (the row key)
 * and string data (table values).
 *)
type column =
{
    data: value Bst.tree;
    last_index: int;
}

(**
 * A table is represented by a TST with string keys (the names of each column)
 * containing columns
 *)
type table = column Tst.tree

(**
 * A database is represented by a TST with string keys (the names of each table)
 * containing tables
 * The mutable updated field contains an Ivar that is filled when a change is
 * made, and is immediately replaced by an unfilled Ivar
 *)
type database =
{
    name: string;
    data: table Tst.tree;
    updated: (database * string) Ivar.t
}

(**
 * results signal whether a database operation was successful or failed.
 * Failures contain a user-facing message to display.
 * P types indicate whether parsing succeeded or failed.
 *)
type result =
    | Success of database
    | Failure of string
    | Column of value list
    | PFailure of string
    | PMessage of string
    | OpColumn of value list list
    | ColNames of string list
    | Keys of key list
    | Json of json
