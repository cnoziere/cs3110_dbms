(* This module handles the creation of databases and tables from files *)
open Types
open Operation
open Yojson.Basic

let rec one_by_one f = function
  | [] -> Success
  | hd :: tl -> (match f hd with
                | Success -> one_by_one f tl
                | Failure s -> Failure s
                | _ -> failwith "This should never pattern match")

let to_column jsoncol =
    let open Yojson.Basic.Util in
    jsoncol
    |> to_list
    |> List.map to_string

let create_full_table json =
    let open Yojson.Basic.Util in
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

    let rows =                          (* rows: string list list *)
        match columns with
        | [] -> []
        | hd :: tl -> let acc = List.map (fun a -> [a]) hd in
                      let f = List.map2 (fun a b -> a @ [b]) in
                      List.fold_left f acc tl in

    let f = add_row table_name column_names in

    match create_table table_name column_names with
    | Failure s -> Failure s
    | Success -> one_by_one f rows
    | _ -> failwith "This should never pattern match"

let create_database json =
    let open Yojson.Basic.Util in
    let db_name =
        json
        |> member "dbName"
        |> to_string in

    let tables =
        [json]
        |> filter_member "tables"
        |> flatten in

    InternalRep.set_name db_name;
    one_by_one create_full_table tables

let read_JSON file =
  match
    try Some (Yojson.Basic.from_file file)
    with _ -> None
  with
  | None -> Failure ("No such file: " ^ file)
  | Some json -> create_database json

(* CHECK ALL FAILWITH NOOOOO *)

(* TO COMPILE: cs3110 compile -t -p async -p yojson readJson.ml *)