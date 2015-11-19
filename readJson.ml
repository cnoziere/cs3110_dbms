(* This module handles the creation of databases and tables from files *)
open Yojson.Basic.Util
open Types
open Operation

(* create the database specified by contents of the file with the given
filename *)
(* returns false if the file cannot be found or a database with that
name already exists *)
let read_JSON = failwith "TODO"
(* Yojson.Basic.from_file file *)


(* creates the database specified by the JSON value *)
(* returns true if creation of the database was successful *)
(* otherwise returns false if the JSON value could not be parsed *)
let create_database = failwith "TODO"

let rec row_by_row f = function
  | [] -> Success
  | hd :: tl -> (match f hd with
                | Success -> row_by_row f tl
                | Failure s -> Failure s)

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

    match Operation.create_table table_name column_names with
    | Failure s -> Failure s
    | Success -> row_by_row f rows