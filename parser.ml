(* cs3110 compile -t -p async -p str parser.ml *)

let exit params = match params with
  | [] -> Printf.printf "%s\n" "Exiting."; false
  | _ -> Printf.printf "%s\n" "Error EXIT: too many parameters."; true

let help params = match params with
  | [] -> Printf.printf "%s\n" "TODO print readme commands."
  | _ -> Printf.printf "%s\n" "Error HELP: too many parameters."

let load params = match params with
  | [] -> Printf.printf "%s\n" "Error LOAD: no filename."
  | h::[] -> failwith "TODO, call readJSON here"
  | _ -> Printf.printf "%s\n" "Error LOAD: too many parameters."

let create_table params = match params with
  | [] -> Printf.printf "%s\n" "Error CREATE TABLE: no column names."
  | _ -> failwith "TODO, call Operation here"

let drop_table params = match params with
  | [] -> Printf.printf "%s\n" "Error DROP TABLE: no tablename."
  | h::[] -> failwith "TODO, call Operation here"
  | _ -> Printf.printf "%s\n" "Error DROP TABLE: too many parameters."

let insert_into params =
  match params with
    | [] -> Printf.printf "%s\n" "Error INSERT INTO: no tablename."
    | h::[] -> Printf.printf "%s\n" "Error INSERT INTO: no columns or values."
    | h::t -> failwith "TODO, split lists and call Operation"
        (* call Operation with h=tablename, result of split_lists*)

let delete_from params = match params with
  | [] -> Printf.printf "%s\n" "Error DELETE FROM: no tablename."
  | h::[] -> failwith "TODO, call Operation here"
  | h::ha::[] when (String.lowercase ha)="where" ->
      Printf.printf "%s\n" "Error DELETE FROM: no WHERE conditions."
  | h::ha::t when (String.lowercase ha)="where" ->
      failwith "TODO, call Operation here"
  | _ -> Printf.printf "%s\n" "Error DELETE FROM: invalid parameters; does not match [tablename] or WHERE."

let update params = match params with
  | [] -> Printf.printf "%s\n" "Error UPDATE: no tablename."
  | h::[] -> Printf.printf "%s\n" "Error UPDATE: no columns or values."
  | h::t -> failwith "TODO, split lists and call Operation"

let evaluate input =
  let word_lst = Str.split (Str.regexp "[ \t]+") input in
  match word_lst with
    | h::t when (String.lowercase h)="exit" -> exit t
    | h::t when (String.lowercase h)="help" -> help t; true
    | h::t when (String.lowercase h)="load" -> load t; true
    | h::ha::t when (String.lowercase h)="create"&&(String.lowercase ha)="table" ->
        create_table t; true
    | h::ha::t when (String.lowercase h)="drop"&&(String.lowercase ha)="table" ->
        drop_table t; true
    | h::ha::t when (String.lowercase h)="insert"&&(String.lowercase ha)="into" ->
        insert_into t; true
    | h::ha::t when (String.lowercase h)="delete"&&(String.lowercase ha)="from" ->
        delete_from t; true
    | h::t when (String.lowercase h)="update" -> update t; true
    | _ -> Printf.printf "%s\n" "Error: command not recognized."; true in

let rec repl () =
  let () = Printf.printf "\n> " in
  let input = read_line () in
  let continue = evaluate input in
  if continue then repl() else () in

let start_repl () =
  let () = Printf.printf "\n%s"
           "Starting DBMS. Type 'help' to see a list of commands." in
  repl() in

start_repl()
