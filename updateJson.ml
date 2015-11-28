open Yojson.Basic

let rec find_nrows (column : string Bst.tree) =
  let open Bst in
  match column with
  | Leaf -> -1
  | Node (row, data, left, right) -> if right = None then row
                                     else find_nrows right

let rec traverse_column (column : string Bst.tree) =
  let open Bst in
  match column with
  | Leaf -> ()
  | Node (row, data, left, right) -> a.(row) <- data;
                                     traverse_column left;
                                     traverse_column right

let parse_column (column : string Bst.tree) =
  let r = find_nrows column + 1 in
  let arr = Array.make r None in
  let f a b =
    match b with
    | None -> a @ "None"
    | Some x -> a @ x in
  (Array.fold_left f [] arr)

let rec traverse_table acc r (table : column Tst.tree) =
  match table with
  | Leaf -> ()
  | Node (ch, col, left, mid, right) ->
    match col with
    | None -> traverse_table "" r left;
              traverse_table (acc ^ (Char.escaped ch)) r mid;
              traverse_table "" r right
    | Some x -> r := ((acc ^ (Char.escaped ch)), (parse_column x)) :: !r

(* converts a table into a JSON value *)
let table_to_json (table : column Tst.tree) (tablename : string) =
  let r = ref [] in
  traverse_table "" r table;        (* r now stores a list of column names to columns *)
  let to_list = List.map (fun a -> `List a) in
  let column_names = List.fold_left (fun a (k,v) -> a @ [`String k]) [] !r) in
  let columns = to_list (List.fold_left (fun a (k,v) -> a @ (to_string v)) [] !r) in

  `Assoc [("tableName", `String tablename);
  ("columnNames", `List column_names);
  ("columns", `List columns)]

let rec traverse_database acc r (database : table Tst.tree) =
  match table with
  | Leaf -> ()
  | Node (ch, col, left, mid, right) ->
    match col with
    | None -> traverse_databse "" r left;
              traverse_database (acc ^ (Char.escaped ch)) r mid;
              traverse_databsse "" r right
    | Some x -> r := (table_to_json x (acc ^ (Char.escaped ch))) :: !r

(* converts a database into a JSON value *)
let database_to_json (database : table Tst.tree) =
  failwith "TODO"
  (* let r = ref [] in
  traverse_database "" r database;
  `Assoc [("dbName", `String databasename);
  ("tables", `List !r)] *)

(* writes a JSON value to the specified file name *)
let JSON_to_file json =
(* Yojson.Basic to_file string json*)

let updated = Ivar.read

(* checks to see if the database has been updated and if so
writes the database to file *)
let watch_for_update d =
  upon (updated d) (fun d' -> watch_for_update d';
    JSON_to_file (database_to_json d))