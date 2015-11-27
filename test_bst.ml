(******************************************************************************)
(** Unit tests for tst Implementation ****************************************)
(******************************************************************************)
open Bst

TEST "INSERT_Key" =
    print_endline "INSERT_Key";
    let t = create () in
    print_string_bst t;
    let (_, t) = insert 2 "hello" t in
    print_string_bst t;
    let (_, t) = insert 1 "world" t in
    print_string_bst t;
    let (duplicate, t) = insert 3 "!" t in
    print_string_bst t;
    not duplicate

TEST "INSERT_Duplicate" =
    print_endline "INSERT_Duplicate";
    let t = create () in
    let (_, t) = insert 2 "hello" t in
    let (_, t) = insert 1 "world" t in
    print_string_bst t;
    let (duplicate, t) = insert 2 "goodbye" t in
    print_string_bst t;
    duplicate

TEST "REMOVE_Key" =
    print_endline "REMOVE_Key";
    let t = create () in
    let (_, t) = insert 42 "hello" t in
    print_string_bst t;
    let (removed, t) = remove 42 t in
    print_string_bst t;
    removed

TEST "REMOVE_Key_One_Child" =
    print_endline "REMOVE_Key_One_Child";
    let t = create () in
    print_string_bst t;
    let (_, t) = insert 5 "a" t in
    let (_, t) = insert 3 "b" t in
    let (_, t) = insert 7 "c" t in
    let (_, t) = insert 1 "d" t in
    let (_, t) = insert 6 "e" t in
    let (_, t) = insert 2 "f" t in
    let (_, t) = insert 4 "g" t in
    print_string_bst t;
    let (removed, t) = remove 7 t in
    print_string_bst t;
    removed

TEST "REMOVE_Key_Two_Children" =
    print_endline "REMOVE_Key_Two_Children";
    let t = create () in
    print_string_bst t;
    let (_, t) = insert 5 "a" t in
    let (_, t) = insert 3 "b" t in
    let (_, t) = insert 7 "c" t in
    let (_, t) = insert 1 "d" t in
    let (_, t) = insert 6 "e" t in
    let (_, t) = insert 2 "f" t in
    let (_, t) = insert 4 "g" t in
    print_string_bst t;
    let (removed, t) = remove 3 t in
    print_string_bst t;
    removed

TEST "REMOVE_From_Leaf" =
    print_endline "REMOVE_From_Leaf";
    let t = create () in
    print_string_bst t;
    let (removed, t) = remove 42 t in
    print_string_bst t;
    not removed

TEST "REMOVE_Nonexistent_Key" =
    print_endline "REMOVE_Nonexistent_Key";
    let t = create () in
    print_string_bst t;
    let (_, t) = insert 42 "hello" t in
    print_string_bst t;
    let (removed, t) = remove 24 t in
    print_string_bst t;
    not removed

TEST "GET_Key" =
    print_endline "GET_Key";
    let t = create () in
    print_string_bst t;
    let (_, t) = insert 42 "hello" t in
    print_string_bst t;
    get 42 t = Some "hello"
