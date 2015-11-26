open Types

let exit params = match params with
  | [] -> (PMessage("Exiting.\n"), false)
  | _ -> (PFailure("Error EXIT: too many parameters."), true)

let help params = match params with
  | [] -> PMessage("TODO print readme commands.")
  | _ -> PFailure("Error HELP: too many parameters.")

let load params = match params with
  | [] -> PFailure("Error LOAD: no filename.")
  | h::[] -> Failure("TODO: call readJson")
  | _ -> PFailure("Error LOAD: too many parameters.")

let create_table params = match params with
  | [] -> PFailure("Error CREATE TABLE: no column names.")
  | _ -> PMessage("TODO, call Operation here")

let drop_table params = match params with
  | [] -> PFailure("Error DROP TABLE: no tablename.")
  | h::[] -> PMessage("TODO, call Operation here")
  | _ -> PFailure("Error DROP TABLE: too many parameters.")

let insert_into params =
  match params with
    | [] -> PFailure("Error INSERT INTO: no tablename.")
    | h::[] -> PFailure("Error INSERT INTO: no columns or values.")
    | h::t -> PMessage("TODO, split lists and call Operation")
        (* call Operation with h=tablename, result of split_lists*)

let delete_from params = match params with
  | [] -> PFailure("Error DELETE FROM: no tablename.")
  | h::[] -> PMessage("TODO, call Operation here")
  | h::ha::[] when (String.lowercase ha)="where" ->
      PFailure("Error DELETE FROM: no WHERE conditions.")
  | h::ha::t when (String.lowercase ha)="where" ->
      PMessage("TODO, call Operation here")
  | _ -> PFailure("Error DELETE FROM: invalid parameters; does not match [tablename] or WHERE.")

let update params = match params with
  | [] -> PFailure("Error UPDATE: no tablename.")
  | h::[] -> PFailure("Error UPDATE: no columns or values.")
  | h::t -> PMessage("TODO, split lists and call Operation")

let evaluate input =
  let word_lst = Str.split (Str.regexp "[ \t]+") input in
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
    | h::ha::t when (String.lowercase h)="delete"&&(String.lowercase ha)="from" ->
        (delete_from t, true)
    | h::t when (String.lowercase h)="update" -> (update t, true)
    | _ -> (PFailure("Error: command not recognized."), true)

let print_result res = match res with
  | Success -> Printf.printf "%s\n" "Success"
  | Failure x -> Printf.printf "%s\n" x
  | PMessage x -> Printf.printf "%s\n" x
  | PFailure x -> Printf.printf "%s\n" x

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
