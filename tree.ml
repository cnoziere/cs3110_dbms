(**
 * Implementation of a ternary search tree
 * Only unique string paths are allowed
 * String keys must not be empty
 *)

(* TODO: alternative Implementation
 * either AVL tree or TST with int list for new keys *)

(* character key for each node *)
type char_key = char

type 'a tree =
    | Leaf
    | Node of (char_key * 'a option * 'a tree * 'a tree * 'a tree)

let create_tree (): 'a tree = Leaf

(**
 * Insert 'a item with a string key and return the updated tree
 * If a value already exists for the string key, replace the stored value
 *)
let rec insert (key: string) (item: 'a option) (t: 'a tree): 'a tree =
    print_endline key;
    match t with
    | Leaf -> (
        if key = "" then failwith "Error: empty keys are not allowed"
        else if (String.length key) = 1 then
            Node (key.[0], item, Leaf, Leaf, Leaf)
        else
            let new_key = String.sub key 1 (String.length key - 1) in
            Node (key.[0], None, Leaf, insert new_key item Leaf, Leaf))
    | Node (c, v, t1, t2, t3) ->
        if key = "" then failwith "Error: empty keys are not allowed"
        else if (key = Char.escaped c) then
            Node (c, item, t1, t2, t3)
        else if (key.[0] = c) then
            let new_key = String.sub key 1 (String.length key - 1) in
            Node (c, v, t1, (insert new_key item t2), t3)
        else if (key.[0] < c) then
            Node (c, v, (insert key item t1), t2, t3)
        else (* key.[0] > c *)
            Node (c, v, t1, t2, (insert key item t3))

(**
 * Returns true if the first character of the string key matches the char_key
 *)
let is_char (t: 'a tree) (key: string): bool =
    match t with
    | Leaf -> false
    | Node (c,_,_,_,_) -> key.[0] = c

(**
 * Remove 'a item with a string key and return the updated tree
 * If string key does not exist, return the original tree
 *)
let rec remove (key: string) (t: 'a tree): 'a tree =
    print_endline key;
    match t with
    | Leaf -> Leaf
    | Node (c, v, t1, t2, t3) ->
        if key = "" then failwith "Error: empty keys are not allowed"
        else if (key = Char.escaped c) then
            Node (c, None, t1, t2, t3)
        else if (key.[0] = c) then
            let new_key = String.sub key 1 (String.length key - 1) in
            if is_char t1 new_key then
                Node (c, v, remove new_key t1, t2, t3)
            else if is_char t2 new_key then
                Node (c, v, t1, remove new_key t2, t3)
            else
                Node (c, v, t1, t2, remove new_key t3)
        else if (key.[0] < c) then
            Node (c, v, (remove key t1), t2, t3)
        else (* key.[0] > c *)
            Node (c, v, t1, t2, (remove key t3))


(**
 * Print tree to terminal for testing
 *)
let print_int_tree (t: int tree): unit =
    let string_int_option = function
        | None -> "None"
        | Some v -> string_of_int v in
    let rec string_int_tree = function
        | Leaf -> "Leaf"
        | Node (c, v, t1, t2, t3) ->
            "Node(\"" ^ (Char.escaped c) ^ "\"," ^ (string_int_option v) ^ ","
            ^ (string_int_tree t1) ^ "," ^ (string_int_tree t2) ^ ","
            ^ (string_int_tree t3) ^ ")" in
    print_endline (string_int_tree t)
