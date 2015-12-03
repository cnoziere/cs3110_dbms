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
  then concat_failures results ""  (* find last success, return that db! *)
  else let rev = List.rev results in List.hd rev

let rec concat_columns results col_lst = match results with
  | (Column x)::t -> concat_columns t (col_lst@[x])
  | _::t -> Failure("Not a column--will not reach this case.")
  | [] -> OpColumn(col_lst)

let filter_columns results =
  if (List.exists (fun x -> match x with | Failure _ -> true | _ -> false) results)
  then concat_failures results "" else concat_columns results []

(**
 * Creates a database, given the database name. Returns Success or Failure.
 *)
let create_database dbname = InternalRep.create_database dbname

(**
 * Creates a table, given the table name and list of column names.
 * Returns a result of Success or Failure.
 *)
let create_table db tname col_names = InternalRep.create_table db tname col_names

(**
 * Deletes a table, given the table name.
 * Returns a result of Success or Failure.
 *)
let drop_table db tname = InternalRep.drop_table db tname

(**
 * Adds a row to a table, given the table name, list of column names,
 * and list of values.
 * Returns a result of Success or Failure.
 *)
let add_row db tname col_names vals = InternalRep.add_row db tname col_names vals

(**
 * Helper functions.
 *)
let get_col_target where =
  match where with
    | None -> ""
    | Some(c,_,_) -> c

let get_f where =
  match where with
    | None -> fun x -> true
    | Some(c, Eq, v) ->     fun x -> x=v
    | Some(c, NotEq, v) ->  fun x -> x<>v
    | Some(c, Gt, v) ->     fun x -> x>v
    | Some(c, Lt, v) ->     fun x -> x<v
    | Some(c, GtEq, v) ->   fun x -> x>=v
    | Some(c, LtEq, v) ->   fun x -> x<=v

(**
 * Deletes a row from a table, given the table name, and a None if no where
 * conditions, or column name, operator, value triple
 * Returns a result of Success if all deletions succeed. If any fail, returns
 * a result of Failure containing messages from all failures.
 *)
let delete_row db tname where =
  let rec delete_key db lst = match lst with
    | [] -> Success(db)
    | h::t -> begin
        let res = InternalRep.delete_row db tname h in
        match res with
          | Failure x -> Failure(x)
          | Success new_db -> delete_key new_db t
          | _ -> Failure("Will not reach this case.")
      end in
  let col_target = get_col_target where in
  let f = get_f where in
  let keys = InternalRep.get_row db tname col_target f in
  match keys with
    | Failure x -> Failure(x)
    | Keys k -> delete_key db k
    | _ -> Failure("Will not reach this case.")

(**
 * Updates a table, given the table name, a list of (column name, value) pairs
 * to identify the row, and the new data as a list of (column name, value
 * pairs.
 * Returns a result of Success if all updates succeed. If any fail, returns
 * a result of Failure containing messages from all failures.
 *)
let update db tname col_lst val_lst where =
  let rec update_over_cols_vals db cols vals key =
    match cols, vals with
      | [], [] -> Success(db)
      | c::cs, v::vs -> begin
          let res = InternalRep.update_value db tname c key v in
          match res with
            | Failure x -> Failure(x)
            | Success new_db -> update_over_cols_vals new_db cs vs key
            | _ -> Failure("Will not reach this case.")
        end
      | _ -> Failure("will not reach this case.") in
  let rec update_key db key_lst = match key_lst with
    | [] -> Success(db)
    | h::t -> begin
        let res = update_over_cols_vals db col_lst val_lst h in
        match res with
          | Failure x -> Failure(x)
          | Success new_db -> update_key new_db t
          | _ -> Failure("Will not reach this case.")
      end in
  let col_target = get_col_target where in
  let f = get_f where in
  let keys = InternalRep.get_row db tname col_target f in
  match keys with
    | Failure x -> Failure(x)
    | Keys k -> update_key db k
    | _ -> Failure("Will not reach this case.")

(**
 * Finds values that match the select-from-where commands. Expects:
 *   - table name
 *   - None if no columns specified, or list of column names
 *   - None if no where conditions, or column name, operator, value triple
 * Returns a result Failure if any selection fails. Otherwise, if all succeed,
 * returns a result OColumn containing the value lists for each selection.
 *)
let select db tname col_names where =
  let col_list = match col_names with
    | None -> InternalRep.get_column_names db tname
    | Some x -> ColNames x in
  let col_target = get_col_target where in
  let f = get_f where in
  match col_list with
    | Failure c -> Failure c
    | ColNames c ->
        let keys = InternalRep.get_row db tname col_target f in
        (match keys with
          | Failure x -> Failure x
          | Keys k -> (let results = List.map (fun x -> InternalRep.get_values db tname x k) c in
                       filter_columns results)
          | _ -> Failure("Will not reach this case."))
    | _ -> Failure("Will not reach this case.")

(**
 * Given a table name, returns result Failure if table doesn't exist, or
 * OpColumn of all columns.
 *)
let get_table db tname =
    let col_names = InternalRep.get_column_names db tname in
    match col_names with
      | Failure x -> Failure x
      | ColNames lst ->
          let results = List.map (fun x -> InternalRep.get_column_vals db tname x (fun y -> true)) lst in
          filter_columns results
      | _ -> Failure("Will not reach this case.")
