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
    let res = not (Sys.file_exists dbname) in
    if res then Unix.mkdir dbname 0o666 else ();
    res

let to_column jsoncol =
    let open Yojson.Basic.Util in
    jsoncol
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
          [json]
          |> filter_member "columnNames"
          |> flatten
          |> List.map to_string in

        let columns =                       (* columns : string list list *)
          [json]
          |> filter_member "columns"
          |> flatten
          |> List.map to_column in

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

let load_table (db : database) (dbname: string) (tablename : string) =
    let path = "./" ^ dbname ^ "/" ^ tablename ^ ".json" in
    match
      (try
        Some (Yojson.Basic.from_file path)
       with
       | _ -> None)
    with
    | Some table -> create_full_table db tablename table
    | None -> Failure ("Cannot find/parse table " ^ tablename ^ ".\n")

let create_database (dbname: string) json =
    let open Yojson.Basic.Util in
    match
      (try
        let dbname =
          json
          |> member "dbName"
          |> to_string in
        let tables =
          [json]
          |> filter_member "tables"
          |> flatten
          |> List.map to_string in
        Some (dbname, tables)
      with
      | _ -> None)
    with
    | None -> Failure ("Cannot parse database " ^ dbname ^ ".\n")
    | Some (dbname, tables) -> (match InternalRep.create_database dbname with
                                | Success db -> one_by_one db (load_table db dbname) tables
                                | Failure s -> Failure s
                                | _ -> failwith "This should never pattern match")

(* filename should be of form __.json *)
let load_db (dbname : string) =
    let path = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
    match
      (try
      Some (Yojson.Basic.from_file path)
      with _ -> None)
    with
    | None -> Failure ("Cannot find/parse the file at the following path: " ^ path ^ "\n")
    | Some json -> create_database dbname json

(* CHECK ALL FAILWITH NOOOOO *)
(* TO COMPILE: cs3110 compile -t -p async -p yojson readJson.ml *)

(* dropping tables ? *)

(* changed read_db to load_db *)

(* wait for asta's implementation in create_full_table *)