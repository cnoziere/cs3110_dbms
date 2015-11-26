open Types
open InternalRep

(**
 * Creates a table, given the table name and list of column names.
 * Returns a type result.
 *)
let create_table tname col_names = InternalRep.create_table tname col_names

(**
 * Deletes a table, given the table name. Returns a type result.
 *)
let drop_table tname = InternalRep.drop_table tname

(**
 * Adds a row to a table, given the table name, list of column names,
 * and list of values. Returns a type result.
 *)
let add_row tname col_names vals = InternalRep.add_row tname col_names vals

(**
 * Deletes a row from a table, given the table name, and a list of
 * (column name, value) pairs to identify the row. Returns a type result.
 *)
let delete_row tname row_id =
  let rec concat_failures results str = match results with
    | (Success)::t -> concat_failures t str
    | (Failure x)::t -> concat_failures t (str ^ x)
    | _::t -> failwith "Will not reach this case."
    | [] -> Failure(str) in
  let rec delete_list keys =
    let results = List.map (fun x -> InternalRep.delete_row tname x) keys in
    if (List.for_all (fun x -> match x with | Failure _ -> true | _ -> false) results)
    then concat_failures results "" else Success in
  match row_id with
    | Some(c, v) -> begin
                      let keys = InternalRep.get_row tname [c] [v] in
                      delete_list keys
                    end
    | None -> begin
                let keys = InternalRep.get_row tname [] [] in
                delete_list keys
              end

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
