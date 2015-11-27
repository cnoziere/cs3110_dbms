open Yojson.Basic

(* type 'a tree =
    | Leaf
    | Node of (char * 'a option * 'a tree * 'a tree * 'a tree)
type table = column tree ref
type column = key tree ref
type key = int *)

let rec find_nrows (column : key tree) =
  match column with
  | Leaf -> -1
  | Node (row, data, left, right) -> if right = None then row
                                     else find_nrows right

let rec traverse_column column =
  match column with
  | Leaf -> ()
  | Node (row, data, left, right) -> a.(row) <- data;
                                     traverse_column left;
                                     traverse_column right

let parse_column column =                  (* string Bst.tree *)
  let r = find_nrows column + 1 in
  let arr = Array.make r None in
  let f a b =
    match b with
    | None -> a @ "None"
    | Some x -> a @ x in
  (Array.fold_left f [] arr)

let rec traverse_table acc r table =
  match table with
  | Leaf -> ()
  | Node (ch, col, left, mid, right) ->
    match col with
    | None -> traverse_table "" r left;
              traverse_table (acc ^ (Char.escaped ch)) r mid;
              traverse_table "" r right
    | Some x -> r := ((acc ^ (Char.escaped ch)), x) :: !r

(* converts a table into a JSON value *)
let table_to_JSON table =                    (* string Bst.tree *)
  let r = ref [] in
  traverse_table "" r table;
  (* r now stores a list of column names to columns *)
  failwith "TODO"


(* converts a database into a JSON value *)
let database_to_JSON d = failwith "TODO"
  List.iter d.tables

(* writes a JSON value to the specified file name *)
let JSON_to_file json = failwith "TODO"
(* Yojson.Basic to_file string json*)

let updated = Ivar.read

(* checks to see if the database has been updated and if so
writes the database to file *)
let watch_for_update d =
  upon (updated d) (fun d' -> watch_for_update d';
    JSON_to_file (database_to_JSON d))