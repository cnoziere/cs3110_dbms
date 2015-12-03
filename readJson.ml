(* This module handles the creation of databases and tables from json files *)
open Types
open Operation
open Yojson.Basic

let rec one_by_one (db : database) f = function
  | [] -> Success db
  | hd :: tl -> (match f hd with
                | Success _ -> one_by_one db f tl
                | Failure s -> Failure s
                | _ -> failwith "This should never pattern match")

let ok_to_create_database (dbname : string) =
    let path = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
    let res = not (Sys.file_exists path) in
    if res then Unix.mkdir dbname 0o777 else ();
    res

let to_string_list json =
    let open Yojson.Basic.Util in
    json
    |> to_list
    |> List.map to_string

let create_full_table (db: database) (tablename : string) json =
    let open Yojson.Basic.Util in
    match
      (try
        let table_name =                    (* table_name : string *)
          json
          |> member "tableName"
          |> to_string in

        let column_names =                  (* column_names : string list *)
          json
          |> member "columnNames"
          |> to_string_list in

        let columns =                       (* columns : string list list *)
          json
          |> member "columns"
          |> to_list
          |> List.map to_string_list in

        Some (table_name, column_names, columns)
      with
      | _ -> None)
    with
    | None -> Failure ("Cannot parse the json file for table " ^ tablename ^ "\n")
    | Some (table_name, column_names, columns) ->
      if List.length columns <> List.length column_names
      then Failure ("Incorrectly formated json file for table " ^ tablename ^
      ". columnNames and column should be of the same length.\n")
      else InternalRep.create_whole_table db table_name column_names columns

let load_table (db : database) (tablename : string) =
    let dbname = db.name in
    let path = "./" ^ dbname ^ "/" ^ tablename ^ ".json" in
    if not (Sys.file_exists path) then Failure ("Cannot find table " ^ tablename
        ^ " in directory " ^ dbname ^ ".\n")
    else match
      (try
        Some (Yojson.Basic.from_file path)
       with
       | _ -> None)
    with
    | Some table -> create_full_table db tablename table
    | None -> Failure ("Cannot parse table " ^ tablename ^ " in directory "
        ^ dbname ^ ".\n")

let rec load_tables tables db =
    match tables with
    | [] -> Success db
    | hd :: tl -> match load_table db hd with
                  | Success db' -> load_tables tl db'
                  | s -> s

let no_failure f = function
    | Failure s -> Failure s
    | Success db -> f db
    | _ -> failwith "This should never pattern match"

let create_database (dbname: string) json =
    let open Yojson.Basic.Util in
    match
      (try
        let dbname =
          json
          |> member "dbName"
          |> to_string in
        let tables =
          json
          |> member "tables"
          |> to_string_list in
        Some (dbname, tables)
      with
      | _ -> None)
    with
    | None -> Failure ("Cannot parse file ./" ^ dbname ^ "/" ^ dbname ^ ".json\n")
    | Some (dbname, tables) -> let x1 = InternalRep.create_database dbname in
                               let x2 = no_failure (load_tables tables) x1 in
                               let f = (fun x -> UpdateJson.watch_for_update x; Success x) in
                               no_failure f x2

let load_db (dbname : string) =
    let path = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
    if not (Sys.file_exists path) then Failure ("Cannot find database " ^ dbname
        ^ " in current directory.\n")
    else match
      (try
      Some (Yojson.Basic.from_file path)
      with _ -> None)
    with
    | Some json -> create_database dbname json
    | None -> Failure ("Cannot parse the following file: " ^ path ^ "\n")

(* let delete_tables db json (path : string) =
    let open Yojson.Basic.Util in
    match
      (try
        let tables =
          json
          |> member "tables"
          |> to_string_list in
        Some tables
      with
      | _ -> None)
    with
    | None -> Failure ("The following file cannot be parsed: " ^ path ^ "\n")
    | Some tables -> List.iter (fun t -> Sys.remove (t ^ ".json")) tables;
                     Success db

let drop_db (db : database) =
    let dbname = db.name in
    let p1 = "./" ^ dbname in
    let p2 = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
    match (Sys.file_exists p1, Sys.file_exists p2) with
    | (true, true) -> (match (try Some (Yojson.Basic.from_file p2) with _ -> None) with
                       | Some json -> (match delete_tables db json p2 with
                                       | Failure s -> Failure s
                                       | Success _ -> Unix.rmdir dbname; Success db)
                       | None -> Failure ("Cannot parse the following file: "
                        ^ path ^ "\n"))
    | (true, false) -> Unix.rmdir dbname; Success db
    | _ -> Success db *)

(* add functionality to remove a database *)
(* check all failwith no *)