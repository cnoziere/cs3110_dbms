(* This module handles the creation of databases and tables from json files *)
open Types
open InternalRep
open Yojson.Basic.Util

let ok_to_create_db (dbname : string) =
  let open Async.Std in
    let path = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
    (Sys.file_exists_exn path) >>= (fun b ->
      if b then return false
      else (Unix.mkdir (~perm:0o777) dbname >>= fun () ->
      Writer.open_file path >>=
      fun t -> Writer.write t (Yojson.Basic.pretty_to_string `Null);
      Writer.close t >>= fun () -> return true))

let to_string_list json =
    let open Yojson.Basic.Util in
    json
    |> to_list
    |> List.map to_string

let create_full_table (db: database) (tablename : string) json =
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
      ". columnNames and columns should be of the same length.\n")
      else create_whole_table db table_name column_names columns

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

let create_db (dbname: string) json =
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
    | Some (dbname, tables) -> let x1 = create_database dbname in
                               let x2 = no_failure (load_tables tables) x1 in
                               let f = (fun x -> ignore(UpdateJson.watch_for_update x);
                               Success x) in no_failure f x2

let load_db (dbname : string) =
    let path = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
    if not (Sys.file_exists path) then Failure ("Cannot find database " ^ dbname
        ^ " in current directory.\n")
    else match
      (try
      Some (Yojson.Basic.from_file path)
      with _ -> None)
    with
    | Some json -> create_db dbname json
    | None -> Failure ("Cannot parse the following file: " ^ path ^ "\n")

let delete_table (p1 : string) (tablename : string) =
  let path = p1 ^ "/" ^ tablename ^ ".json" in
  if Sys.file_exists path then Sys.remove path else ()

let drop_db (db : database) =
    let dbname = db.name in
    let p1 = "./" ^ dbname in
    let p2 = "./" ^ dbname ^ "/" ^ dbname ^ ".json" in
    match (Sys.file_exists p1, Sys.file_exists p2) with
    | (true, true) -> let tablenames = get_table_names db in
                      List.iter (delete_table p1) tablenames;
                      Success db
    | (true, false) -> Unix.rmdir dbname; Success db
    | _ -> Failure ("Database " ^ db.name ^ "does not exist.\n")