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
