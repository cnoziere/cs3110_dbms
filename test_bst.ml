(******************************************************************************)
(** Unit tests for tst Implementation ****************************************)
(******************************************************************************)
open Bst

TEST_UNIT "INSERT_Key" =
    print_endline "INSERT_Key";
    let t = create () in
    print_string_bst t;
    let t = insert 2 "hello" t in
    print_string_bst t;
    let t = insert 1 "world" t in
    print_string_bst t;
    let t = insert 3 "!" t in
    print_string_bst t

TEST_UNIT "INSERT_Duplicate" =
    print_endline "INSERT_Duplicate";
    let t = create () in
    print_string_bst t;
    let t = insert 2 "hello" t in
    print_string_bst t;
    let t = insert 1 "world" t in
    print_string_bst t;
    let t = insert 2 "goodbye" t in
    print_string_bst t

TEST_UNIT "REMOVE_Key" =
    print_endline "REMOVE_Key";
    let t = create () in
    print_string_bst t;
    let t = insert 42 "hello" t in
    print_string_bst t;
    let t = remove 42 t in
    print_string_bst t

TEST "GET_Key" =
    print_endline "GET_Key";
    let t = create () in
    print_string_bst t;
    let t = insert 42 "hello" t in
    print_string_bst t;
    get 42 t = Some "hello"
