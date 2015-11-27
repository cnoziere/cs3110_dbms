(******************************************************************************)
(** Unit tests for Tree Implementation ****************************************)
(******************************************************************************)
open Tst
open Bst

TEST_UNIT "INSERT_Char" =
    print_endline "INSERT_Char";
    let t = create_tree () in
    print_int_tree t;
    let t = insert "a" (Some 42) t in
    print_int_tree t

TEST_UNIT "INSERT_Key" =
    print_endline "INSERT_Key";
    let t = create_tree () in
    print_int_tree t;
    let t = insert "hello" (Some 42) t in
    print_int_tree t;
    let t = insert "hi" (Some 24) t in
    print_int_tree t

TEST_UNIT "INSERT_Duplicate" =
    print_endline "INSERT_Duplicate";
    let t = create_tree () in
    print_int_tree t;
    let t = insert "ll" (Some 42) t in
    print_int_tree t;

TEST_UNIT "REMOVE_Char" =
    print_endline "REMOVE_Char";
    let t = create_tree () in
    print_int_tree t;
    let t = insert "a" (Some 24) t in
    print_int_tree t;
    let t = remove "a" t in
    print_int_tree t

TEST_UNIT "REMOVE_Key" =
    print_endline "REMOVE_Key";
    let t = create_tree () in
    print_int_tree t;
    let t = insert "hello" (Some 24) t in
    print_int_tree t;
    let t = remove "hello" t in
    print_int_tree t
