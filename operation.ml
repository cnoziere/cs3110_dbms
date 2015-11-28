open Types
open InternalRep

(**
 * Creates a table, given the table name and list of column names.
 * Returns a result of Success or Failure.
 *)
let create_table tname col_names = InternalRep.create_table tname col_names

(**
 * Deletes a table, given the table name.
 * Returns a result of Success or Failure.
 *)
let drop_table tname = InternalRep.drop_table tname

(**
 * Adds a row to a table, given the table name, list of column names,
 * and list of values.
 * Returns a result of Success or Failure.
 *)
let add_row tname col_names vals = InternalRep.add_row tname col_names vals

(**
 * Helper functions. [check_failures] is for [delete_row] and [update].
 * [filter_columns] used by [select_from] and [select_from_where].
 *)
let rec concat_failures results str = match results with
  | (Failure x)::t -> concat_failures t (str ^ x)
  | _::t -> concat_failures t str
  | [] -> Failure(str)

let check_failures results =
  if (List.exists (fun x -> match x with | Failure _ -> true | _ -> false) results)
  then concat_failures results "" else Success

let rec concat_columns results col_lst = match results with
  | (Column x)::t -> concat_columns t (x::col_lst)
  | _::t -> failwith "Will not reach this case."
  | [] -> OpColumn(col_lst)

let filter_columns results =
  if (List.exists (fun x -> match x with | Failure _ -> true | _ -> false) results)
  then concat_failures results "" else concat_columns results []

(**
 * Deletes a row from a table, given the table name, and a list of
 * (column name, value) pairs to identify the target rows.
 * Returns a result of Success if all deletions succeed. If any fail, returns
 * a result of Failure containing messages from all failures.
 *)
let delete_row tname row_id =
  let delete_list keys =
    let results = List.map (fun x -> InternalRep.delete_row tname x) keys in
    check_failures results in
  match row_id with
    | Some(c, v) -> delete_list (InternalRep.get_row tname [c] [v])
    | None -> delete_list (InternalRep.get_row tname [] [])

(**
 * Updates a table, given the table name, a list of (column name, value) pairs
 * to identify the row, and the new data as a list of (column name, value
 * pairs.
 * Returns a result of Success if all updates succeed. If any fail, returns
 * a result of Failure containing messages from all failures.
 *)
let update tname col_lst val_lst row_id =
  let update_one key =
    List.map2 (fun x y -> InternalRep.update_value tname x key y) col_lst val_lst in
  let update_list keys =
    let results = List.flatten (List.map (fun x -> update_one x) keys) in
    check_failures results in
  match row_id with
    | Some(c, v) -> update_list (InternalRep.get_row tname [c] [v])
    | None -> update_list (InternalRep.get_row tname [] [])

(**
 * Finds values that match the select-from command, given column names
 * (select) and table name (from).
 * Returns a result Failure if any selection fails. Otherwise, if all succeed,
 * returns a result OColumn the value lists for each selection.
 *)
let select_from col_names tname =
  let col_list = match col_names with
    | None -> InternalRep.get_column_list tname
    | Some x -> x in
    let results = List.map (fun x -> InternalRep.get_column tname x) col_list in
    filter_columns results

(**
 * Finds values that match the select-from-where commands, given column names
 * list (select), table name (from), and triple of column name, operator,
 * value (where) that results must match.
 * Returns a result Failure if any selection fails. Otherwise, if all succeed,
 * returns a result OColumn containing the value lists for each selection.
 *)
let select_from_where (*col_names tname operator col val*) =
  (* let fun_to_check = match operator with
    | Eq -> (fun x -> x=val)
    | NotEq -> (fun x -> x<>val)
    | Gt -> (fun x -> x>val)
    | Lt -> (fun x -> x<val)
    | GtEq -> (fun x -> x>=val)
    | LtEq -> (fun x -> x<=val) in *)
  failwith "TODO"
