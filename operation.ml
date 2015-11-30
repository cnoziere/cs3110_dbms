open Types

(**
 * Helper functions. [check_failures] is for [delete_row] and [update].
 * [filter_columns] used by [select_from] and [select_from_where].
 *)
let rec concat_failures results str = match results with
  | (Failure x)::t -> concat_failures t (str ^ " " ^ x)
  | _::t -> concat_failures t str
  | [] -> Failure(str)

let check_failures results =
  if (List.exists (fun x -> match x with | Failure _ -> true | _ -> false) results)
  then concat_failures results "" else Success

let rec concat_columns results col_lst = match results with
  | (Column x)::t -> concat_columns t (col_lst@[x])
  | _::t -> Failure("Not a column--will not reach this case.")
  | [] -> OpColumn(col_lst)

let filter_columns results =
  if (List.exists (fun x -> match x with | Failure _ -> true | _ -> false) results)
  then concat_failures results "" else concat_columns results []

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
 * Deletes a row from a table, given the table name, and a None if no where
 * conditions, or column name, operator, value triple
 * Returns a result of Success if all deletions succeed. If any fail, returns
 * a result of Failure containing messages from all failures.
 *)
let delete_row tname where =
  let col = match where with
    | None -> ""
    | Some(c, _, _) -> c in
  let f = match where with
    | None -> fun x -> true
    | Some(c, Eq, v) ->     fun x -> x=v
    | Some(c, NotEq, v) ->  fun x -> x<>v
    | Some(c, Gt, v) ->     fun x -> x>v
    | Some(c, Lt, v) ->     fun x -> x<v
    | Some(c, GtEq, v) ->   fun x -> x>=v
    | Some(c, LtEq, v) ->   fun x -> x<=v in
  let keys = InternalRep.get_row tname col f in
  let delete_key key = InternalRep.delete_row tname key in
  let results = match keys with
    | Keys(x) -> List.map delete_key x
    | _ -> [Failure("Will not reach this case.")] in
  check_failures results

(**
 * Updates a table, given the table name, a list of (column name, value) pairs
 * to identify the row, and the new data as a list of (column name, value
 * pairs.
 * Returns a result of Success if all updates succeed. If any fail, returns
 * a result of Failure containing messages from all failures.
 *)
let update tname col_lst val_lst where =
  let col = match where with
    | None -> ""
    | Some(c, _, _) -> c in
  let f = match where with
    | None -> fun x -> true
    | Some(c, Eq, v) ->     fun x -> x=v
    | Some(c, NotEq, v) ->  fun x -> x<>v
    | Some(c, Gt, v) ->     fun x -> x>v
    | Some(c, Lt, v) ->     fun x -> x<v
    | Some(c, GtEq, v) ->   fun x -> x>=v
    | Some(c, LtEq, v) ->   fun x -> x<=v in
  let keys = InternalRep.get_row tname col f in
  let update_key key = List.map2 (fun x y -> InternalRep.update_value tname x key y) col_lst val_lst in
  let results = match keys with
    | Keys(x) -> List.map update_key x
    | _ -> [[Failure("Will not reach this case.")]] in
  check_failures (List.flatten results)

(**
 * Finds values that match the select-from-where commands. Expects:
 *   - table name
 *   - None if no columns specified, or list of column names
 *   - None if no where conditions, or column name, operator, value triple
 * Returns a result Failure if any selection fails. Otherwise, if all succeed,
 * returns a result OColumn containing the value lists for each selection.
 *)
let select tname col_names where =
  let col_list = match col_names with
    | None -> InternalRep.get_column_names tname
    | Some x -> ColNames x in
  let f = match where with
    | None -> fun x -> true
    | Some(c, Eq, v) ->     fun x -> x=v
    | Some(c, NotEq, v) ->  fun x -> x<>v
    | Some(c, Gt, v) ->     fun x -> x>v
    | Some(c, Lt, v) ->     fun x -> x<v
    | Some(c, GtEq, v) ->   fun x -> x>=v
    | Some(c, LtEq, v) ->   fun x -> x<=v in
  match col_list with
    | Failure c -> Failure c
    | ColNames c ->
        let results = List.map (fun x -> InternalRep.get_column_vals tname x f) c in (* another col name... *)
        filter_columns results
    | _ -> Failure("Will not reach this case.")

let get_table tname =
    let col_names = InternalRep.get_column_names tname in
    match col_names with
      | Failure x -> Failure x
      | ColNames lst ->
          let results = List.map (fun x -> InternalRep.get_column_vals tname x (fun y -> true)) lst in
          filter_columns results
      | _ -> Failure("Will not reach this case.")
