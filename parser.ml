open Types

(**
 * Helper functions for parsing user input and starting evaluation.
 * Well formed user input begins evaluation with calls to Operation or ReadJson.
 * Invalid syntax causes a parse failure instead.
 * See README file and test_parser for examples of valid input.
 *)

let exit params = match params with
  | [] -> (PMessage("Exiting.\n"), false)
  | _ -> (PFailure("Error EXIT: too many parameters."), true)

let help params = match params with
  | [] -> PMessage("todo: print readme commands.")
  | _ -> PFailure("Error HELP: too many parameters.")

let load params = match params with
  | [] -> PFailure("Error LOAD: no filename.")
  | h::[] -> Failure("todo: ReadJson.read_JSON h "^h)
  | _ -> PFailure("Error LOAD: too many parameters.")

let create_table params = match params with
  | [] -> PFailure("Error CREATE TABLE: no table name.")
  | h::[] -> PFailure("Error CREATE TABLE: no column names.")
  | h::t -> Failure("todo: Operation.create_table h t "^h^" "^(List.hd t))

let drop_table params = match params with
  | [] -> PFailure("Error DROP TABLE: no tablename.")
  | h::[] -> Failure("todo: Operation.drop_table h "^h)
  | _ -> PFailure("Error DROP TABLE: too many parameters.")

(**
 * Parse "INSERT INTO tablename (col1, col2, ...) VALUES (val1, val2, ...)"
 * Start by splitting on keyword VALUES, to get [col1; col2; ...] and
 *   [val1; val2; ...]
 * Then check validity of lists and call Operation or return Parse Failure.
 *)
let insert_into params =
  let rec split_lists lst is_before cols vals = match lst with
    | [] -> (cols, vals)
    | h::t when is_before -> begin
        if (String.lowercase h)="values" then split_lists t false cols vals
        else split_lists t true (h::cols) vals
      end
    | h::t -> split_lists t false cols (h::vals) in
  match params with
    | [] -> PFailure("Error INSERT INTO: no tablename.")
    | h::[] -> PFailure("Error INSERT INTO: no columns or values.")
    | h::t -> begin
        let lists = split_lists t true [] [] in
        let cols = fst lists in
        let vals = snd lists in
        if cols=[] then PFailure("Error INSERT INTO: no column names.")
        else if vals=[] then PFailure("Error INSERT INTO: no values.")
        else if (List.length cols)<>(List.length vals) then
          PFailure("Error INSERT INTO: number of columns does not match values.")
        else Failure("todo: Operation.add_row h (fst lists) (snd lists) "^h^" "^ (List.hd cols)^" "^(List.hd vals))
      end

(** Helper function for [delete_from] and [update]. *)
type where = NoSpec | Invalid | Valid of string*string
let parse_where lst =
  if lst=[] then NoSpec else
  let param_str = String.concat "" lst in
  let to_match  = Str.regexp "\\([A-Za-z0-9]+='[A-Za-z0-9 ]+'\\)" in
  if Str.string_match to_match param_str 0 then begin
    let to_remove = Str.regexp "\\('\\)" in
    let no_quotes = Str.global_replace to_remove "" param_str in
    let split_on = Str.regexp "\\(=\\)" in
    let final_list = Str.split split_on no_quotes in
    Valid(List.nth final_list 0, List.nth final_list 1)
    end
  else Invalid

(**
 * First match "DELETE FROM tablename" and "DELETE * FROM tablename" formats.
 * Then parse "DELETE FROM tablename WHERE col='val'" formats.
 * Validate tablename, parse where conditions, and return Parse Failure or
 *    call Operation.
 *)
let delete_from params =
  let parse_from params = match params with
    | [] -> PFailure("Error DELETE FROM: no tablename.")
    | h::[] -> PFailure("Error DELETE FROM: no WHERE conditions.")
    | h::ha::[] when (String.lowercase ha)="where" ->
        PFailure("Error DELETE FROM: no WHERE conditions.")
    | h::ha::t when (String.lowercase ha)="where" -> begin
        let parsed_where = parse_where t in
        (match parsed_where with
          | NoSpec -> Failure("todo: Operation.delete_row h None "^h)
          | Invalid -> PFailure("Error DELETE FROM: invalid WHERE conditions.")
          | Valid(f,s) -> Failure("todo: Operation.delete_row h Some(f,s) "^h^" "^f^ " "^s)
        )
      end
    | _ -> PFailure("Error DELETE FROM: parameters must match [tablename WHERE].") in
  match params with
    | h::ha::hb::[] when h="*"&&(String.lowercase ha)="from" ->
        Failure("todo: Operation.delete_row hb None "^hb)
    | h::ha::t when h="*"&&(String.lowercase ha)="from" ->
        PFailure("Error DELETE FROM: too many tablename parameters.")
    | h::t when (String.lowercase h)="from" -> parse_from t
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
let update params =
  let rec split_lists lst is_before pairs where = match lst with
    | [] -> (pairs, where)
    | h::t when is_before -> begin
        if (String.lowercase h)="where" then split_lists t false pairs where
        else split_lists t true (h::pairs) where
      end
    | h::t -> split_lists t false pairs (h::where) in
  let split_set set =
    let match_fst = Str.regexp "\\([A-Za-z0-9]+=\\)" in
    let match_snd = Str.regexp "\\(='[A-Za-z0-9]+'\\)" in
    let match_all = Str.regexp "\\([A-Za-z0-9]+='[A-Za-z0-9 ]+'\\)" in
    let split_on = Str.regexp "\\(=\\)" in
    let rec clean lst result = match lst with
      | [] -> result
      | h::t when Str.string_match match_fst h 0 ->
          clean t (Str.split split_on h)@result
      | h::t when Str.string_match match_snd h 0 ->
          clean t (Str.split split_on h)@result
      | h::t when Str.string_match match_all h 0 ->
          clean t (Str.split split_on h)@result
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
    | h::ha::t when (String.lowercase ha)="set"->
        let lists = split_lists t true [] [] in
        let set = fst lists in
        let where = snd lists in
        let parsed_set = split_set set in
        let new_cols = fst parsed_set in
        let new_vals = snd parsed_set in
        let parsed_where = parse_where where in
        if new_cols=[] then PFailure("Error UPDATE: invalid SET")
        else if new_vals=[] then PFailure("Error UPDATE: invalid SET")
        else if (List.length new_cols)<>(List.length new_vals) then
          PFailure("Error UPDATE: invalid SET")
        else (match parsed_where with
          | NoSpec -> Failure("todo: Operation.update h new_cols new_vals None "^h^" "^(List.hd new_cols)^" "^(List.hd new_vals))
          | Invalid -> PFailure("Error UPDATE: invalid WHERE")
          | Valid(f,s) -> Failure("todo: Operation.update h new_cols new_vals Some(f,s) "^h^" "^(List.hd new_cols)^" "^(List.hd new_vals)^" "^f^" "^s)
        )
    | h::t -> PFailure("Error UPDATE: must match SET and WHERE.")

(**
 * First match "SELECT * FROM tablename" formats.
 * Then parse "SELECT (col1, col2, ...) FROM tablename" formats.
 * Start by splitting on keyword FROM, to get [col1; col2; ...]
 *   and [tablename]
 * Then check validity of lists and call Operation or return Parse Failure.
 *)
let select params =
  let rec split_lists lst is_before cols tname = match lst with
    | [] -> (cols, tname)
    | h::t when is_before -> begin
        if (String.lowercase h)="from" then split_lists t false cols tname
        else split_lists t true (h::cols) tname
      end
    | h::t -> split_lists t false cols (h::tname) in
  let parse_lists params = match params with
    | xs -> begin
        let lists = split_lists xs true [] [] in
        let col = fst lists in
        let tname = snd lists in
        if tname=[] then PFailure("Error SELECT: no tablename.")
        else if (List.length tname)>1 then
          PFailure("Error SELECT: too many tablename parameters.")
        else if col=[] then PFailure("Error SELECT: no column names.")
        else Failure("todo: Operation.select_from col (List.hd tname) "^(List.hd col)^" "^(List.hd tname))
      end in
  match params with
    | [] -> PFailure("Error SELECT: no columns or values.")
    | h::ha::hb::[] when h="*"&&(String.lowercase ha)="from" ->
        Failure("todo: Operation.select_from col None hb "^hb)
    | h::ha::t when h="*"&&(String.lowercase ha)="from" ->
        PFailure("Error SELECT: invalid tablename parameters.")
    | xs -> parse_lists xs

(**
 * Main function for parsing user input and evaluation.
 * All keywords are case insensitive.
 *)

let evaluate input =
  let word_lst = Str.split (Str.regexp "[ \t,()]+") input in
  match word_lst with
    | h::t when (String.lowercase h)="exit" -> exit t
    | h::t when (String.lowercase h)="help" -> (help t, true)
    | h::t when (String.lowercase h)="load" -> (load t, true)
    | h::ha::t when (String.lowercase h)="create"&&(String.lowercase ha)="table" ->
        (create_table t, true)
    | h::ha::t when (String.lowercase h)="drop"&&(String.lowercase ha)="table" ->
        (drop_table t, true)
    | h::ha::t when (String.lowercase h)="insert"&&(String.lowercase ha)="into" ->
        (insert_into t, true)
    | h::t when (String.lowercase h)="delete" -> (delete_from t, true)
    | h::t when (String.lowercase h)="update" -> (update t, true)
    | h::t when (String.lowercase h)="select" -> (select t, true)
    | _ -> (PFailure("Error: command not recognized."), true)

(** Functions to print results from evaluation. *)

let print_cols col_lst = failwith "TODO"

let print_result res = match res with
  | Success -> Printf.printf "%s\n" "Success"
  | Failure x -> Printf.printf "%s\n" x
  | PMessage x -> Printf.printf "%s\n" x
  | PFailure x -> Printf.printf "%s\n" x
  | OpColumn x -> print_cols x
  | _ -> Printf.printf "%s\n" "Could not print--will not reach this case."

(**
 * Functions to start REPL. Handles the Read and Loop parts.
 * Calls helper functions to Evaluate and Print.
 *)

let rec repl () =
  let () = Printf.printf "\n> " in
  let input = read_line () in
  let evaluated = evaluate input in
  let () = print_result (fst evaluated) in
  let continue = snd evaluated in
  if continue then repl() else ()

let start_repl () =
  let () = Printf.printf "\n%s"
           "Starting DBMS. Type 'help' to see a list of commands." in
  repl()
