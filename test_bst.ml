(******************************************************************************)
(** Unit tests for tst Implementation ****************************************)
(******************************************************************************)
open Bst

let rec lists_match x y =
    match x, y with
    | h1::t1, h2::t2 -> h1 = h2 && (lists_match t1 t2)
    | [],[] -> true
    | _,_ -> false

TEST "INSERT_Key" =
    print_endline "INSERT_Key";
    let t = create () in
    let (_, t) = insert 2 "hello" t in
    let (_, t) = insert 1 "world" t in
    let (duplicate, t) = insert 3 "!" t in
    get 3 t = Some "!" && get 2 t = Some "hello"
    && get 1 t = Some "world" && not duplicate

TEST "INSERT_Duplicate" =
    print_endline "INSERT_Duplicate";
    let t = create () in
    let (_, t) = insert 2 "hello" t in
    let (_, t) = insert 1 "world" t in
    let (duplicate, t) = insert 2 "goodbye" t in
    get 2 t = Some "goodbye" && duplicate

TEST "REMOVE_Key" =
    print_endline "REMOVE_Key";
    let t = create () in
    let (_, t) = insert 42 "hello" t in
    let (removed, t) = remove 42 t in
    get 42 t = None && removed

TEST "REMOVE_Key_One_Child" =
    print_endline "REMOVE_Key_One_Child";
    let t = create () in
    let (_, t) = insert 5 "a" t in
    let (_, t) = insert 3 "b" t in
    let (_, t) = insert 7 "c" t in
    let (_, t) = insert 1 "d" t in
    let (_, t) = insert 6 "e" t in
    let (_, t) = insert 2 "f" t in
    let (_, t) = insert 4 "g" t in
    let (removed, t) = remove 7 t in
    get 7 t = None && removed

TEST "REMOVE_Key_Two_Children" =
    print_endline "REMOVE_Key_Two_Children";
    let t = create () in
    let (_, t) = insert 5 "a" t in
    let (_, t) = insert 3 "b" t in
    let (_, t) = insert 7 "c" t in
    let (_, t) = insert 1 "d" t in
    let (_, t) = insert 6 "e" t in
    let (_, t) = insert 2 "f" t in
    let (_, t) = insert 4 "g" t in
    let (removed, t) = remove 3 t in
    get 3 t = None && removed

TEST "REMOVE_From_Leaf" =
    print_endline "REMOVE_From_Leaf";
    let t = create () in
    let (removed, t) = remove 42 t in
    get 42 t = None && not removed

TEST "REMOVE_Nonexistent_Key" =
    print_endline "REMOVE_Nonexistent_Key";
    let t = create () in
    let (_, t) = insert 42 "hello" t in
    let (removed, t) = remove 24 t in
    get 24 t = None && not removed

TEST "GET_Key" =
    print_endline "GET_Key";
    let t = create () in
    let (_, t) = insert 42 "hello" t in
    get 42 t = Some "hello"

TEST "LIST_bst" =
    print_endline "LIST_bst";
    let t = create () in
    let (_, t) = insert 5 "a" t in
    let (_, t) = insert 3 "b" t in
    let (_, t) = insert 7 "c" t in
    let (_, t) = insert 1 "d" t in
    let (_, t) = insert 6 "e" t in
    let (_, t) = insert 2 "f" t in
    let (_, t) = insert 4 "g" t in
    lists_match (list_bst t) [(1, "d"); (2, "f"); (3, "b"); (4, "g");
    (5, "a"); (6, "e"); (7, "c")]
