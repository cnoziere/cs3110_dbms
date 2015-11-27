(* This module handles the creation of databases and tables from files *)
open Types
open Operation
open Yojson.Basic

let rec one_by_one f = function
  | [] -> Success
  | hd :: tl -> (match f hd with
                | Success -> one_by_one f tl
                | Failure s -> Failure s
                | _ -> failwith "no")

let to_row jsonrow =
    let open Yojson.Basic.Util in
    jsonrow
    |> to_list
    |> List.map to_string

let create_full_table json =
    let open Yojson.Basic.Util in
    let table_name =                (* table_name : string *)
        json
        |> member "tableName"
        |> to_string in

    let column_names =              (* column_names : string list *)
        [json]
        |> filter_member "columnNames"
        |> flatten
        |> List.map to_string in

    let rows =                      (* rows : string list list *)
        [json]
        |> filter_member "rows"
        |> flatten
        |> List.map to_row in

    let f = add_row table_name column_names in

    match create_table table_name column_names with
    | Failure s -> Failure s
    | Success -> one_by_one f rows
    | _ -> failwith "no"

let create_database json =
    let open Yojson.Basic.Util in
    (* let db_name =
        json
        |> member "dbName"
        |> to_string in *)

    let tables =
        [json]
        |> filter_member "tables"
        |> flatten in

    one_by_one create_full_table tables

let read_JSON file =
  match
    try Some (Yojson.Basic.from_file file)
    with _ -> None
  with
  | None -> Failure ("No such file or directory: " ^ file)
  | Some json -> create_database json

(* CHECK ALL FAILWITH NOOOOO *)

(* TO COMPILE: cs3110 compile -t -p async -p yojson readJson.ml *)