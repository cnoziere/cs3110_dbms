open Types

let exit params = match params with
  | [] -> (Message("Exiting.\n"), false)
  | _ -> (Failure("Error EXIT: too many parameters."), true)

let help params = match params with
  | [] -> Message("TODO print readme commands.")
  | _ -> Failure("Error HELP: too many parameters.")

let load params = match params with
  | [] -> Failure("Error LOAD: no filename.")
  | h::[] -> Message("TODO, call readJSON here")
  | _ -> Failure("Error LOAD: too many parameters.")

let create_table params = match params with
  | [] -> Failure("Error CREATE TABLE: no column names.")
  | _ -> Message("TODO, call Operation here")

let drop_table params = match params with
  | [] -> Failure("Error DROP TABLE: no tablename.")
  | h::[] -> Message("TODO, call Operation here")
  | _ -> Failure("Error DROP TABLE: too many parameters.")

let insert_into params =
  match params with
    | [] -> Failure("Error INSERT INTO: no tablename.")
    | h::[] -> Failure("Error INSERT INTO: no columns or values.")
    | h::t -> Message("TODO, split lists and call Operation")
        (* call Operation with h=tablename, result of split_lists*)

let delete_from params = match params with
  | [] -> Failure("Error DELETE FROM: no tablename.")
  | h::[] -> Message("TODO, call Operation here")
  | h::ha::[] when (String.lowercase ha)="where" ->
      Failure("Error DELETE FROM: no WHERE conditions.")
  | h::ha::t when (String.lowercase ha)="where" ->
      Message("TODO, call Operation here")
  | _ -> Failure("Error DELETE FROM: invalid parameters; does not match [tablename] or WHERE.")

let update params = match params with
  | [] -> Failure("Error UPDATE: no tablename.")
  | h::[] -> Failure("Error UPDATE: no columns or values.")
  | h::t -> Message("TODO, split lists and call Operation")

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
    | _ -> (Failure("Error: command not recognized."), true)

let print_result res = match res with
  | Success -> Printf.printf "%s\n" "Success"
  | Message x -> Printf.printf "%s\n" x
  | Failure x -> Printf.printf "%s\n" x

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
