open Types
open Async.Std

(**
 * Helper functions for parsing user input and starting evaluation.
 * Well formed user input begins evaluation with calls to Operation or ReadJson.
 * Invalid syntax causes a parse failure instead.
 * See README file and test_parser for examples of valid input.
 *)

let stdin : Reader.t = Lazy.force Reader.stdin

let help_message =
"\n
--------------------------------------------------------------------------------
EXIT
  - Exits the REPL.

HELP
  - Displays a list of commands with help on valid formats for parameters.

LOAD filename
  - Loads the backed up database contained in the JSON file with name filename.

CREATE DATABASE dbname
  - Creates a database with name dbname.
  - *** Note: There must be a database before doing operations on tables.

CREATE TABLE tablename (col1,col2,...)
  - Creates a table with name tablename and columns col1, col2, ...

DROP TABLE tablename
  - Drops the table with name tablename

INSERT INTO tablename (col1,col2,...) VALUES (val1,val2,...)
  - Inserts a new row into table with name tablename. Sets column name col1 to
  value val1, col2 to val2, ...

DELETE FROM tablename WHERE []
  - Deletes rows from table with name tablename.
  - Identifies the rows to delete by the WHERE condition. See below for help on
  WHERE.

DELETE * FROM tablename
  - Another way to delete rows. This deletes all rows from the table with name
  tablename.

UPDATE tablename SET (col1=val1, col2=val2, ...) WHERE []
  - Updates rows in the table with name tablename. Updates column name col1 to
  value val1, col2 to val2, …
  - Identifies the rows to update by the WHERE condition. See below for help on
  WHERE.

SELECT (col1,col2,...) FROM tablename WHERE []
  - Selects, and prints to terminal, the columns col1, col2, … from the table
with name tablename.
  - Identifies the rows to print from by the WHERE condition. See below for
  help on WHERE.

SELECT * FROM tablename
  - Another way to select rows. This selects, and prints to terminal, every
  column in the table.

WHERE column_name OPERATOR value
  - WHERE filters rows for other operations. Rows that satisfy column_name
  OPERATOR value are selected.
  - OPERATOR must be one of the following:
    =
    <>
    >
    <
    >=
    <=

PRINT tablename
  - Prints the table with name tablename to the terminal.
--------------------------------------------------------------------------------"

let exit params = match params with
  | [] -> (PMessage("Exiting.\n"), false)
  | _ -> (PFailure("Error EXIT: too many parameters."), true)

let help params = match params with
  | [] -> PMessage(help_message)
  | _ -> PFailure("Error HELP: too many parameters.")

let load params = match params with
  | [] -> PFailure("Error LOAD: no filename.")
  | h::[] -> ReadJson.load_db h
  | _ -> PFailure("Error LOAD: too many parameters.")

let create_database params = match params with
  | [] -> return (PFailure "Error CREATE DATABASE: no name.")
  | h::[] ->
      ReadJson.ok_to_create_db h >>= fun b ->
      if b then
        let res = Operation.create_database h in
        (match res with
          | Success db -> ignore(UpdateJson.watch_for_update db)
          | _ -> ());
        return res
      else return (Failure ("Error: Database " ^ h ^ " already exists."))
  | _ -> return (PFailure "Error CREATE DATABASE: too many parameters.")

let create_table db params = match params with
  | [] -> PFailure("Error CREATE TABLE: no table name.")
  | h::[] -> PFailure("Error CREATE TABLE: no column names.")
  | h::t -> Operation.create_table db h t

let drop_table db params = match params with
  | [] -> PFailure("Error DROP TABLE: no tablename.")
  | h::[] -> Operation.drop_table db h
  | _ -> PFailure("Error DROP TABLE: too many parameters.")

(**
 * Parse "INSERT INTO tablename (col1, col2, ...) VALUES (val1, val2, ...)"
 * Start by splitting on keyword VALUES, to get [col1; col2; ...] and
 *   [val1; val2; ...]
 * Then check validity of lists and call Operation or return Parse Failure.
 *)
let insert_into db params =
  let rec split_lists lst is_before cols vals = match lst with
    | [] -> (cols, vals)
    | h::t when is_before ->
        if (String.lowercase h) = "values" then split_lists t false cols vals
        else split_lists t true (cols @ [h]) vals
    | h::t -> split_lists t false cols (vals @ [h]) in

  match params with
    | [] -> PFailure("Error INSERT INTO: no tablename.")
    | h::[] -> PFailure("Error INSERT INTO: no columns or values.")
    | h::t ->
        let lists = split_lists t true [] [] in
        let cols = fst lists in
        let vals = snd lists in
        if cols = [] then PFailure("Error INSERT INTO: no column names.")
        else if vals = [] then PFailure("Error INSERT INTO: no values.")
        else if (List.length cols) <> (List.length vals) then
          PFailure("Error INSERT INTO: number of columns does not match values.")
        else Operation.add_row db h cols vals

(**
 * Helper function to parse WHERE statements.
 * Expects everything after the keyword WHERE.
 * Returns None for invalid forms, Some (col_name, op, value)
 *)
let parse_where lst =
  if lst = [] then None else
  let param_str = String.concat "" lst in

  let eq  =   Str.regexp ("\\([A-Za-z0-9]+=[A-Za-z0-9]+\\)" ^ "$") in
  let noteq = Str.regexp ("\\([A-Za-z0-9]+<>[A-Za-z0-9]+\\)" ^ "$") in
  let gt =    Str.regexp ("\\([A-Za-z0-9]+>[A-Za-z0-9]+\\)" ^ "$") in
  let lt =    Str.regexp ("\\([A-Za-z0-9]+<[A-Za-z0-9]+\\)" ^ "$") in
  let gteq =  Str.regexp ("\\([A-Za-z0-9]+>=[A-Za-z0-9]+\\)" ^ "$") in
  let lteq =  Str.regexp ("\\([A-Za-z0-9]+<=[A-Za-z0-9]+\\)" ^ "$") in

  if Str.string_match eq param_str 0 then
    let strs = Str.split (Str.regexp "\\(=\\)") param_str in
    Some(List.nth strs 0, Eq, List.nth strs 1)
  else if Str.string_match noteq param_str 0 then
    let strs = Str.split (Str.regexp "\\(<>\\)") param_str in
    Some(List.nth strs 0, NotEq, List.nth strs 1)
  else if Str.string_match gt param_str 0 then
    let strs = Str.split (Str.regexp "\\(>\\)") param_str in
    Some(List.nth strs 0, Gt, List.nth strs 1)
  else if Str.string_match lt param_str 0 then
    let strs = Str.split (Str.regexp "\\(<\\)") param_str in
    Some(List.nth strs 0, Lt, List.nth strs 1)
  else if Str.string_match gteq param_str 0 then
    let strs = Str.split (Str.regexp "\\(>=\\)") param_str in
    Some(List.nth strs 0, GtEq, List.nth strs 1)
  else if Str.string_match lteq param_str 0 then
    let strs = Str.split (Str.regexp "\\(<=\\)") param_str in
    Some(List.nth strs 0, LtEq, List.nth strs 1)
  else None

(**
 * First match "DELETE FROM tablename" and "DELETE * FROM tablename" formats.
 * Then parse "DELETE FROM tablename WHERE col='val'" formats.
 * Validate tablename, parse where conditions, and return Parse Failure or
 *    call Operation.
 *)
let delete_from db params =
  let parse_from params = match params with
    | [] -> PFailure("Error DELETE FROM: no tablename.")
    | h::[] -> PFailure("Error DELETE FROM: no WHERE conditions.")
    | h::ha::[] when (String.lowercase ha)="where" ->
        PFailure("Error DELETE FROM: no WHERE conditions.")
    | h::ha::t when (String.lowercase ha)="where" ->
        let where = parse_where t in
        (match where with
          | None -> PFailure("Error DELETE FROM: invalid WHERE conditions.")
          | Some(c,o,v) -> Operation.delete_row db h (Some(c,o,v)))
    | _ -> PFailure("Error DELETE FROM: parameters must match [tablename WHERE].") in

  match params with
    | h::ha::hb::[] when h = "*" && (String.lowercase ha) = "from" ->
        Operation.delete_row db hb None
    | h::ha::t when h = "*" && (String.lowercase ha) = "from" ->
        PFailure("Error DELETE FROM: too many tablename parameters.")
    | h::t when (String.lowercase h) = "from" -> parse_from t
    | _ -> PFailure("Error DELETE FROM: invalid parameters.")

(**
 * Parse "UPDATE tablename SET (col1='val1', col2='val2', ...) WHERE col='val'"
 * Start by splitting on keyword WHERE, to get [col1='val1'; col2='val2'; ...]
 *  and col='val'
 * Split first list to get [col1; col2; ...] and [val1; val2; ...]
 * Parse col='val' to get type where
 * Then check validity of col and val lists. Return Parse Failure or
 *   call Operation depending on type where
 *)
let update db params =
  let rec split_lists lst is_before pairs where = match lst with
    | [] -> (pairs, where)
    | h::t when is_before ->
        if (String.lowercase h) = "where"
        then split_lists t false pairs where
        else split_lists t true (pairs @ [h]) where
    | h::t -> split_lists t false pairs (where @ [h]) in

  let split_set set =
    let match_fst = Str.regexp "\\([A-Za-z0-9]+=\\)" in
    let match_snd = Str.regexp "\\(='[A-Za-z0-9]+'\\)" in
    let match_all = Str.regexp "\\([A-Za-z0-9]+='[A-Za-z0-9 ]+'\\)" in
    let split_on = Str.regexp "\\(=\\)" in
    let rec clean lst result = match lst with
      | [] -> result
      | h::t when Str.string_match match_fst h 0 ->
          clean t (Str.split split_on h) @ result
      | h::t when Str.string_match match_snd h 0 ->
          clean t (Str.split split_on h) @ result
      | h::t when Str.string_match match_all h 0 ->
          clean t (Str.split split_on h) @ result
      | _ -> [] in
    let rec extract lst cols vals = match lst with
      | [] -> (cols, vals)
      | h::ha::t -> extract t (h::cols) (ha::vals)
      | _ -> ([], []) in
    let cleaned = clean set [] in
    extract cleaned [] [] in

  match params with
    | [] -> PFailure("Error UPDATE: no tablename.")
    | h::[] -> PFailure("Error UPDATE: no columns or values.")
    | h::ha::t when (String.lowercase ha) = "set"->
        let lists = split_lists t true [] [] in
        let set = fst lists in
        let after_where = snd lists in
        let parsed_set = split_set set in
        let new_cols = fst parsed_set in
        let new_vals = snd parsed_set in
        let where = parse_where after_where in
        if new_cols = [] then PFailure("Error UPDATE: invalid SET")
        else if new_vals = [] then PFailure("Error UPDATE: invalid SET")
        else if (List.length new_cols) <> (List.length new_vals) then
          PFailure("Error UPDATE: invalid SET")
        else (match where with
          | None -> PFailure("Error UPDATE: invalid WHERE")
          | Some(c,o,v) -> Operation.update db h new_cols new_vals (Some(c,o,v)))
    | h::t -> PFailure("Error UPDATE: must match SET and WHERE.")

(**
 * First match "SELECT * FROM tablename" formats.
 * Then parse "SELECT (col1, col2, ...) FROM tablename" formats.
 * Start by splitting on keyword FROM, to get [col1; col2; ...]
 *   and [tablename]
 * Then check validity of lists and call Operation or return Parse Failure.
 *)
let select db params =
  let rec split_lists lst is_before cols tname = match lst with
    | [] -> (cols, tname)
    | h::t when is_before -> begin
        if (String.lowercase h) = "from" then split_lists t false cols tname
        else split_lists t true (cols @ [h]) tname
      end
    | h::t -> split_lists t false cols (tname @ [h]) in
  let parse_lists params =
    let lists = split_lists params true [] [] in
    let col = fst lists in
    let tname = snd lists in
    if col = [] then PFailure("Error SELECT: no column names.") else
    (match tname with
      | [] -> PFailure("Error SELECT: no tablename.")
      | h::[] -> Operation.select db h (Some(col)) None
      | h::ha::t when (String.lowercase ha) = "where" ->
          (let where = parse_where t in
          (match where with
            | None -> PFailure("Error SELECT: invalid WHERE conditions.")
            | Some(c,o,v) -> Operation.select db h (Some(col)) (Some(c,o,v))))
      | _ -> PFailure("Error SELECT: too many tablename parameters.")) in
  match params with
    | [] -> PFailure("Error SELECT: no columns or values.")
    | h::ha::hb::[] when h = "*" && (String.lowercase ha) = "from" ->
        Operation.select db hb None None
    | h::ha::hb::hc::t when h = "*" && (String.lowercase ha) = "from" && (String.lowercase hc) = "where" ->
        let where = parse_where t in
        (match where with
          | None -> PFailure("Error SELECT: invalid WHERE conditions.")
          | Some(c,o,v) -> Operation.select db hb None (Some(c,o,v)))
    | h::ha::t when h = "*" && (String.lowercase ha) = "from" ->
        PFailure("Error SELECT: invalid tablename parameters.")
    | xs -> parse_lists xs

(**
 * Gets an entire table as an OpColumn result, to be printed by print_result.
 *)
let print db name = match name with
  | [] -> PFailure("Error PRINT: no table name.")
  | h::[] -> TRes(h, Operation.get_table db h)
  | _ -> PFailure("Error PRINT: too many parameters.")

(**
 * Main function for parsing user input and evaluation.
 * All keywords are case insensitive.
 *)

let to_normal (d, b) =
  d >>= fun r -> return (r, b)

let evaluate_db db input =
  let word_lst = Str.split (Str.regexp "[ \t,()]+") input in
  match word_lst with
    | h::t when (String.lowercase h) = "exit" -> return (exit t)
    | h::t when (String.lowercase h) = "help" -> return (help t, true)
    | h::t when (String.lowercase h) = "load" -> return (load t, true)
    | h::ha::t when (String.lowercase h) = "create" && (String.lowercase ha) = "database" ->
        to_normal (create_database t, true)
    | h::ha::t when (String.lowercase h) = "create" && (String.lowercase ha) = "table" ->
        return (create_table db t, true)
    | h::ha::t when (String.lowercase h) = "drop" && (String.lowercase ha) = "table" ->
        return (drop_table db t, true)
    | h::ha::t when (String.lowercase h) = "insert" && (String.lowercase ha) = "into" ->
        return (insert_into db t, true)
    | h::t when (String.lowercase h) = "delete" -> return (delete_from db t, true)
    | h::t when (String.lowercase h) = "update" -> return (update db t, true)
    | h::t when (String.lowercase h) = "select" -> return (select db t, true)
    | h::t when (String.lowercase h) = "print" -> return (print db t, true)
    | _ -> return (PFailure("Error: command not recognized."), true)

let evaluate input =
  let db_fail = (PFailure("Error: must load or create a database first."), true) in
  let word_lst = Str.split (Str.regexp "[ \t,()]+") input in
  match word_lst with
    | h::t when (String.lowercase h) = "exit" -> return (exit t)
    | h::t when (String.lowercase h) = "help" -> return (help t, true)
    | h::t when (String.lowercase h) = "load" -> return (load t, true)
    | h::ha::t when (String.lowercase h) = "create" && (String.lowercase ha) = "database" ->
        to_normal (create_database t, true)
    | h::ha::t when (String.lowercase h) = "create" && (String.lowercase ha) = "table" ->
        return db_fail
    | h::ha::t when (String.lowercase h) = "drop" && (String.lowercase ha) = "table" ->
        return db_fail
    | h::ha::t when (String.lowercase h) = "insert" && (String.lowercase ha) = "into" ->
        return db_fail
    | h::t -> let b = (String.lowercase h) = "delete" ||
                      (String.lowercase h) = "update" ||
                      (String.lowercase h) = "select" ||
                      (String.lowercase h) = "print" in
              if b then return db_fail
              else return (PFailure("Error: command not recognized."), true)
    | _ -> return (PFailure("Error: command not recognized."), true)

(** Functions to print results from evaluation. *)

let cols_to_rows col_list =
  let f = fun acc c -> match c with
                       | hd :: tl -> acc @ [hd]
                       | [] -> failwith "no" in
  let f' = fun c -> match c with
                     | hd :: tl -> tl
                     | [] -> failwith "no" in
  let rec helper lst rows =
  match lst with
  | [] -> rows
  | hd :: tl -> if hd = [] then rows
                else let new_rows = rows @ [List.fold_left f [] lst] in
                helper (List.map f' lst) new_rows in
  helper col_list []

let print_row row =
  let rec print_rec lst = match lst with
    | [] -> printf "%s" "\n"
    | h::[] -> printf ", %s\n" h
    | h::t -> printf ", %s" h; print_rec t in
  match row with
    | [] -> printf "%s" "No values"
    | h::t -> printf "%s" h; print_rec t

let print_cols db tablename col_lst =
  match InternalRep.get_column_names db tablename with
  | ColNames column_names ->
      let rec helper lst = match lst with
                           | [] -> ()
                           | h::[] -> printf "%s" h
                           | h::t -> printf "%s, " h; helper t in
      helper column_names;
      let rows = cols_to_rows col_lst in
      printf "%s" "\n";
      List.iter print_row rows
  | _ -> ()

let print_cols_reg col_lst =
  let rows = cols_to_rows col_lst in
  Printf.printf "\n%s" "";
  List.iter print_row rows

let print_result res = match res with
  | Success _ -> printf "%s\n" "Success"
  | Failure x -> printf "%s\n" x
  | PMessage x -> printf "%s\n" x
  | PFailure x -> printf "%s\n" x
  | OpColumn x -> print_cols_reg x
  | _ -> printf "%s\n" "Could not print."

(**
 * Functions to start REPL. Handles the Read and Loop parts.
 * Calls helper functions to Evaluate and Print.
 *)
let rec repl_db db =
  printf "\n> ";
  Reader.read_line stdin >>= fun i ->
  match i with
  | `Eof -> repl_db db
  | `Ok input -> evaluate_db db input >>= fun evaluated ->
                 let first = fst evaluated in
                 let _ = (match first with
                          | TRes (a, (OpColumn x)) -> print_cols db a x
                          | _ -> print_result first) in
                 let continue = snd evaluated in
                 let database = match first with
                                | Success x -> x
                                | _ -> db in
                 if continue then repl_db database
                 else let () = ignore(Async.Std.exit 0) in return ()

let rec repl () =
  printf "\n> ";
  Reader.read_line stdin >>= fun i ->
  match i with
  | `Eof -> repl ()
  | `Ok input -> evaluate input >>= fun evaluated ->
                 let first = fst evaluated in
                 print_result first;
                 let continue = snd evaluated in
                 let database = match first with
                                | Success x -> Some x
                                | _ -> None in
                 if continue then
                 (match database with
                 | Some x -> repl_db x
                 | None -> repl ())
                 else let () = ignore(Async.Std.exit 0) in return ()

let _ = printf "\n%s" "Starting DBMS. Type HELP to see a list of commands."
let _ = repl ()
let _ = Scheduler.go ()
