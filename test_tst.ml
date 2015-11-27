(******************************************************************************)
(** Unit tests for tst Implementation ****************************************)
(******************************************************************************)
open Tst

TEST_UNIT "INSERT_Char" =
    print_endline "INSERT_Char";
    let t = create () in
    print_int_tst t;
    let t = insert "a" 42 t in
    print_int_tst t

TEST_UNIT "INSERT_Key" =
    print_endline "INSERT_Key";
    let t = create () in
    print_int_tst t;
    let t = insert "hello" 42 t in
    print_int_tst t;
    let t = insert "hi" 42 t in
    print_int_tst t

TEST_UNIT "INSERT_Duplicate" =
    print_endline "INSERT_Duplicate";
    let t = create () in
    print_int_tst t;
    let t = insert "ll" 42 t in
    print_int_tst t;

TEST_UNIT "REMOVE_Char" =
    print_endline "REMOVE_Char";
    let t = create () in
    print_int_tst t;
    let t = insert "a" 42 t in
    print_int_tst t;
    let t = remove "a" t in
    print_int_tst t

TEST_UNIT "REMOVE_Key" =
    print_endline "REMOVE_Key";
    let t = create () in
    print_int_tst t;
    let t = insert "hello" 42 t in
    print_int_tst t;
    let t = remove "hello" t in
    print_int_tst t

TEST "GET_Key" =
    print_endline "GET_Key";
    let t = create () in
    print_int_tst t;
    let t = insert "hello" 42 t in
    print_int_tst t;
    get "hello" t = Some 42
