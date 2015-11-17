(******************************************************************************)
(** Unit tests for Tree Implementation ****************************************)
(******************************************************************************)
open Tree

TEST_UNIT "INSERT_Leaf" =
    let t = create_tree () in
    print_int_tree t;
    let t = insert "hi" (Some 42) t in
    print_int_tree t
