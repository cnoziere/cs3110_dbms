open InternalRep
open Types

let rec lists_match x y =
    match x, y with
    | h1::t1, h2::t2 -> h1 = h2 && (lists_match t1 t2)
    | [],[] -> true
    | _,_ -> false

let print_string_list = List.iter print_endline

(* Set up a database *)
let myDB =
    match create_database "(reset myDB)" with
    | Success db ->
    (match create_table db "addrow" ["name"; "grade"; "age"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["asta"; "college"; "19"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["kathy"; "11"; "15"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["bob"; "8"; "13"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["max"; "2"; "7"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["andy"; "3"; "8"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["john"; "none"; "23"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["amanda"; "college"; "18"] with
    | Success db ->
    (match add_row db "addrow" ["name"; "grade"; "age"] ["constance"; "college"; "21"] with
    | Success db -> db
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error")
    | _ -> failwith "Error"


(*
let print_table (table_name: string) =
    match get_column_names table_name with
    | ColNames col_names ->
        let rec print_column = function
        | [] -> ()
        | col_name::t ->
            Printf.printf "COLUMN: %s\n" col_name;
            (match get_column_vals table_name col_name (fun x -> true) with
            | Column x -> print_string_list x
            | Failure msg -> print_endline msg
            | _ -> print_endline "Error 1");
            print_column t in
        print_column col_names
    | Failure msg -> print_endline msg
    | _ -> print_endline "Error 2"
*)

TEST "CREATE_DATABASE_returns_success" =
    print_endline "CREATE_DATABASE_returns_success";
    match create_database "(reset myDB)" with
    | Success db -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "SET_NAME_returns_success" =
    print_endline "SET_NAME_returns_success";
    match create_database "(reset myDB)" with
    | Success db ->
        (get_name db = "(reset myDB)" &&
        match set_name "newDB" db with
        | Success db -> get_name db = "newDB"
        | Failure msg -> print_endline msg; false
        | _ -> false)
    | Failure msg -> print_endline msg; false
    | _ -> false


TEST "CREATE_TABLE_returns_success" =
    print_endline "CREATE_TABLE_returns_success";
    match create_database "(reset myDB)" with
    | Success db ->
        (match create_table db "test" ["a"; "b"; "c"] with
        | Success db -> true
        | Failure msg -> print_endline msg; false
        | _ -> false)
    | Failure msg -> print_endline msg; false
    | _ -> false


TEST "CREATE_TABLE_duplicate_table" =
    print_endline "CREATE_TABLE_duplicate_table";
    match create_database "(reset myDB)" with
    | Success db ->
        (match create_table db "test" ["a"; "b"; "c"] with
        | Success db ->
            (match create_table db "test" ["a"; "b"; "c"] with
            | Failure "Table name already exists in database" -> true
            | _ -> false)
        | _ -> false)
    | _ -> false


TEST "CREATE_TABLE_duplicate_col" =
    print_endline "CREATE_TABLE_duplicate_col";
    match create_database "(reset myDB)" with
    | Success db ->
        (match create_table db "test" ["a"; "c"; "c"] with
        | Failure "Duplicate column name used to initialize table" -> true
        | _ -> false)
    | _ -> false


TEST "DROP_table_add" =
    print_endline "DROP_table_add";
    match create_database "(reset myDB)" with
    | Success db ->
        (match create_table db "test" ["a"; "b"; "c"] with
        | Success db ->
            (match drop_table db "test" with
            | Success db ->
                (match create_table db "test" ["a"; "b"; "c"] with
                | Success db -> true
                | Failure msg -> print_endline msg; false
                | _ -> false)
            | Failure msg -> print_endline msg; false
            | _ -> false)
        | _ -> false)
    | _ -> false


TEST "ADD_ROW_success" =
    print_endline "ADD_ROW_success";
    match create_database "(reset myDB)" with
    | Success db ->
        (match create_table db "addrow" ["name"; "grade"; "age"] with
        | Success db ->
            (match add_row db "addrow" ["name"; "grade"; "age"] ["asta"; "college"; "19"] with
            | Success db -> true
            | Failure msg -> print_endline msg; false
            | _ -> false)
        | _ -> false)
    | _ -> false

TEST "ADD_ROW_partial_list" =
    print_endline "ADD_ROW_partial_list";
    match add_row (reset myDB) "addrow" ["name"; "age"] ["asta"; "19"] with
    | Success db ->
        (match add_row db "addrow" ["age"; "grade"] ["20"; "college"] with
        | Success db -> true
        | Failure msg -> print_endline msg; false
        | _ -> false)
    | Failure msg -> print_endline msg; false
    | _ -> false


TEST "ADD_ROW_no_table_failure" =
    print_endline "ADD_ROW_no_table_failure";
    match add_row (reset myDB) "not_table" ["name"; "age"] ["asta"; "19"] with
    | Failure _ -> true
    | _ -> false

TEST "ADD_ROW_diff_length" =
    print_endline "ADD_ROW_diff_length";
    match create_database "(reset myDB)" with
    | Success db ->
        (match create_table db "addrow" ["name"; "grade"; "age"] with
        | Success db ->
            (match add_row db "not_table" ["name"; "age"] ["asta"] with
            | Failure _ -> true
            | _ -> false)
        | _ -> false)
    | _ -> false


TEST "DELETE_ROW_success" =
    print_endline "DELETE_ROW_success";
    match delete_row (reset myDB) "addrow" 4 with
    | Success db -> true
    | Failure msg -> print_endline msg; false
    | _ -> false


TEST "DELETE_ROW_failure" =
    print_endline "DELETE_ROW_failure";
    match delete_row (reset myDB) "addrow" 20 with
    | Failure "Key does not exist" -> true
    | _ -> false


TEST "UPDATE_VALUE_success" =
    print_endline "UPDATE_VALUE_success";
    match update_value (reset myDB) "addrow" "age" 2 "20" with
    | Success db -> true
    | Failure msg -> print_endline msg; false
    | _ -> false


TEST "UPDATE_VALUE_no_key" =
    print_endline "UPDATE_VALUE_no_key";
    match update_value (reset myDB) "addrow" "age" 20 "20" with
    | Failure _ -> true
    | _ -> false


TEST "GET_COLUMN_NAMES_return_all" =
    print_endline "GET_COLUMN_NAMES_return_all";
    match get_column_names (reset myDB) "addrow" with
    | ColNames x -> lists_match x ["age"; "grade"; "name"]
    | Failure msg -> print_endline msg; false
    | _ -> false

(*

TEST "GET_COLUMN_VALS_return_all_vals" =
    print_endline "GET_COLUMN_VALS_return_all_vals";
    match get_column_vals "getcol" "name" (fun x -> true) with
    | Column x -> lists_match x [""; "asta"; "john"; "max"; "kathy"; "bob"]
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "GET_COLUMN_VALS_return_equals" =
    print_endline "GET_COLUMN_VALS_return_equals";
    match get_column_vals "getcol" "age" (fun x -> x = "11") with
    | Column x -> lists_match x ["11"]
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "GET_ROW_returns_all_keys_age" =
    print_endline "GET_ROW_returns_all_keys_age";
    match get_row "getcol" "age" (fun x -> x = "11" || x = "10") with
    | Keys x -> lists_match x [2;3]
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "GET_ROW_returns_all_keys" =
    print_endline "GET_ROW_returns_all_keys";
    match get_row "getcol" "" (fun x -> true) with
    | Keys x -> lists_match x [0;1;2;3;4;5]
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "GET_VALUES_success" =
    print_endline "GET_ROW_returns_all_keys";
    match get_row "getcol" "age" (fun x -> x = "11" || x = "10") with
    | Keys keys ->
        (match get_values "getcol" "age" keys with
        | Column vals -> lists_match vals ["10";"11"]
        | Failure msg -> print_endline msg; false
        | _ -> false)
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "GET_TABLE_NAMES_success" =
    print_endline "GET_TABLE_NAMES_success";
    lists_match (get_table_names ()) ["addrow"; "b"; "getcol"; "test"]

TEST_UNIT "PRINT_TABLE" =
    print_endline "PRINT_TABLE";
    print_table "getcol";
    ignore (update_value "getcol" "age" 4 "15");
    ignore (update_value "getcol" "name" 0 "test");
    print_table "getcol"

*)
