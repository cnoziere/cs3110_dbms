(******************************************************************************)
(** Unit tests for tst Implementation ****************************************)
(******************************************************************************)
open Tst

TEST "INSERT_Char" =
    print_endline "INSERT_Char";
    let t = create () in
    print_int_tst t;
    let (flag, t) = insert "a" 42 t in
    print_int_tst t;
    not flag

TEST "INSERT_Key" =
    print_endline "INSERT_Key";
    let t = create () in
    let (_, t) = insert "hello" 42 t in
    print_int_tst t;
    let (flag, t) = insert "hi" 42 t in
    print_int_tst t;
    not flag

TEST "INSERT_Duplicate_Char" =
    print_endline "INSERT_Duplicate_Char";
    let t = create () in
    print_int_tst t;
    let (flag, t) = insert "ll" 42 t in
    print_int_tst t;
    not flag

TEST "INSERT_Duplicate_Key" =
    print_endline "INSERT_Duplicate_Key";
    let t = create () in
    let (_, t) = insert "ll" 42 t in
    let (flag, t) = insert "ll" 24 t in
    print_int_tst t;
    get "ll" t = Some 24 && flag

TEST "INSERT_Empty_Key" =
    print_endline "INSERT_Empty_Key";
    let t = create () in
    let (flag, t) = insert "" 42 t in
    print_int_tst t;
    not flag

TEST "REMOVE_Key_Reinsert" =
    print_endline "REMOVE_Key_Reinsert";
    let t = create () in
    let (_, t) = insert "hi" 42 t in
    let (_, t) = remove "hi" t in
    let (flag, t) = insert "hi" 42 t in
    not flag

TEST "REMOVE_Char" =
    print_endline "REMOVE_Char";
    let t = create () in
    print_int_tst t;
    let (_, t) = insert "a" 42 t in
    print_int_tst t;
    let (flag, t) = remove "a" t in
    print_int_tst t;
    flag

TEST "REMOVE_Key" =
    print_endline "REMOVE_Key";
    let t = create () in
    print_int_tst t;
    let (_, t) = insert "hello" 42 t in
    print_int_tst t;
    let (flag, t) = remove "hello" t in
    print_int_tst t;
    flag

TEST "REMOVE_Nonexistent_Key" =
    print_endline "REMOVE_Nonexistent_Key";
    let t = create () in
    print_int_tst t;
    let (_, t) = insert "hello" 42 t in
    print_int_tst t;
    let (flag, t) = remove "hell" t in
    print_int_tst t;
    not flag

TEST "GET_Key" =
    print_endline "GET_Key";
    let t = create () in
    print_int_tst t;
    let (_, t) = insert "hello" 42 t in
    print_int_tst t;
    get "hello" t = Some 42

TEST_UNIT "PRINT_Keys" =
    print_endline "PRINT_Keys";
    let t = create () in
    let (_, t) = insert "a" 1 t in
    let (_, t) = insert "ab" 2 t in
    let (_, t) = insert "abs" 3 t in
    let (_, t) = insert "john" 4 t in
    let (_, t) = insert "asta" 5 t in
    let (_, t) = insert "josh" 6 t in
    let (_, t) = insert "shells" 7 t in
    let (_, t) = insert "she" 8 t in
    let (_, t) = insert "sells" 9 t in
    let (_, t) = insert "sea" 10 t in
    let (_, t) = insert "seashells" 11 t in
    let (_, t) = insert "by" 12 t in
    (*
    (a, 1)
    (ab, 2)
    (abs, 3)
    (asta, 5)
    (by, 12)
    (john, 4)
    (josh, 6)
    (sea, 10)
    (seashells, 11)
    (sells, 9)
    (she, 8)
    (shells, 7)
    *)
    print_keys (keys_tst t)


TEST "GET_Keys" =
    print_endline "GET_Keys";
    let t = create () in
    let (_, t) = insert "name" 1 t in
    let (_, t) = insert "age" 2 t in
    match get "name" t with
    | None -> false
    | Some x -> print_int x; true
