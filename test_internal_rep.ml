open InternalRep
(* open Async.Std *)
open Types


(*
(* Test the deferred d is determined to be the value v *)
let test_async_eq (d: 'a Deferred.t) (v: 'a): bool =
    Thread_safe.block_on_async (fun () -> d) = Core.Std.Result.Ok v

(* Test the deferred d is determined to be the value v within timespan t *)
let test_async_eq_with_timeout (t: float) (d: 'a Deferred.t) (v: 'a): bool =
    Thread_safe.block_on_async (fun ()
    -> with_timeout (Core.Std.sec t) d) = Core.Std.Result.Ok (`Result v)

(* Test the deferred d is not determined to be the value v within timespan t *)
let test_async_timeout (t: float) (d: 'a Deferred.t): bool =
    Thread_safe.block_on_async (fun ()
    -> with_timeout (Core.Std.sec t) d) = Core.Std.Result.Ok `Timeout

(* Test if the value at val_ref is as expected after time delay *)
let check_val (val_ref: int ref) (expected: int) (delay: float): bool =
    Thread_safe.block_on_async (fun () -> after (Core.Std.sec delay)
    >>= fun () -> return !val_ref) = Core.Std.Result.Ok expected

(* Test if the values at ref1 and ref2 are as expected after time delay *)
let check_two_vals (ref1: int ref) (val1: int)
        (ref2: int ref) (val2: int) (t: float): bool =
    Thread_safe.block_on_async (fun () -> after (Core.Std.sec t) >>=
    fun () -> return (!ref1, !ref2)) = Core.Std.Result.Ok (val1, val2)
*)

let rec lists_match x y =
    match x, y with
    | h1::t1, h2::t2 -> h1 = h2 && (lists_match t1 t2)
    | [],[] -> true
    | _,_ -> false

TEST "CREATE_TABLE_returns_success" =
    print_endline "CREATE_TABLE_returns_success";
    match create_table "test" ["a"; "b"; "c"] with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "CREATE_TABLE_duplicate_table" =
    print_endline "CREATE_TABLE_duplicate_table";
    match create_table "test" ["d"; "e"; "f"] with
    | Failure "Table name already exists in database" -> true
    | _ -> false

TEST "CREATE_TABLE_duplicate_col" =
    print_endline "CREATE_TABLE_duplicate_col";
    match create_table "test2" ["d"; "e"; "f"; "e"] with
    | Failure "Duplicate column name used to initialize table" -> true
    | _ -> false

(*
TEST_UNIT "UPDATED_not_determined" =
    print_endline "UPDATED_not_determined";
    upon (updated ()) (fun () -> print_endline "Print: UPDATED_not_determined")

TEST "UPDATED_determined" =
    print_endline "UPDATED_determined";
    upon (updated ()) (fun () -> print_endline "Print: UPDATED_determined");
    match create_table "a" ["a"; "b"; "c"] with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false
*)

TEST "DROP_added_table" =
    print_endline "DROP_added_table";
    ignore (create_table "b" ["a"; "b"; "c"]);
    match drop_table "b" with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "DROP_table_add" =
    print_endline "DROP_table_add";
    ignore (create_table "b" ["a"; "b"; "c"]);
    ignore (drop_table "b");
    match create_table "b" ["a"; "b"; "c"] with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "ADD_ROW_success" =
    print_endline "ADD_ROW_success";
    ignore (create_table "addrow" ["name"; "grade"; "age"]);
    match add_row "addrow" ["name"; "grade"] ["asta"; "college"] with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "ADD_ROW_partial_list" =
    print_endline "ADD_ROW_partial_list";
    ignore(add_row "addrow" ["age"] ["5"]);
    ignore(add_row "addrow" ["name"; "grade"; "age"] ["asta"; "10"; "15"]);
    match add_row "addrow" ["grade"] ["college"] with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "ADD_ROW_no_table_failure" =
    print_endline "ADD_ROW_no_table_failure";
    match add_row "not_table" ["grade"] ["college"] with
    | Failure _ -> true
    | _ -> false

TEST "ADD_ROW_failure" =
    print_endline "ADD_ROW_failure";
    match add_row "addrow" ["year"] ["college"] with
    | Failure _ -> true
    | _ -> false

TEST "DELETE_ROW_success" =
    print_endline "DELETE_ROW_success";
    match delete_row "addrow" 1 with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "DELETE_ROW_failure" =
    print_endline "DELETE_ROW_failure";
    match delete_row "addrow" 5 with
    | Failure "Key does not exist" -> true
    | _ -> false

TEST "UPDATE_VALUE_success" =
    print_endline "UPDATE_VALUE_success";
    match update_value "addrow" "age" 2 "19" with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "UPDATE_VALUE_success_first_col" =
    print_endline "UPDATE_VALUE_success_first_col";
    match update_value "addrow" "name" 2 "hello" with
    | Success -> true
    | Failure msg -> print_endline msg; false
    | _ -> false

TEST "UPDATE_VALUE_no_key" =
    print_endline "UPDATE_VALUE_no_key";
    match update_value "addrow" "age" 8 "19" with
    | Failure _ -> true
    | _ -> false

TEST "GET_COLUMN_NAMES_return_all" =
    print_endline "GET_COLUMN_NAMES_return_all";
    ignore(create_table "getcol" ["name"; "age"]);
    ignore(add_row "getcol" ["age"] ["5"]);
    ignore(add_row "getcol" ["name"; "age"] ["asta"; "4"]);
    ignore(add_row "getcol" ["name"; "age"] ["john"; "10"]);
    ignore(add_row "getcol" ["name"; "age"] ["max"; "11"]);
    ignore(add_row "getcol" ["name"] ["kathy"]);
    ignore(add_row "getcol" ["name"; "age"] ["bob"; "20"]);
    match get_column_names "getcol" with
    | ColNames x -> lists_match x [" "; "age"; "name"]
    | Failure msg -> print_endline msg; false
    | _ -> false

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

TEST "GET_ROW_returns_all_keys" =
    print_endline "GET_ROW_returns_all_keys";
    match get_row "getcol" "age" (fun x -> x = "11" || x = "10") with
    | Keys x -> lists_match x [2;3]
    | Failure msg -> print_endline msg; false
    | _ -> false


(* let _ = Scheduler.go () *)
