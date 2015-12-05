(**
 * Implementation of a ternary search tree
 * Only unique string paths are allowed
 * If key is empty "", replace key with one whitespace " "
 *)

(* Each node contains a character key, an 'a value, and three subtrees *)
type 'a tree =
    | Leaf
    | Node of (char * 'a option * 'a tree * 'a tree * 'a tree)

(**
 * Returns true if the first character of the string key matches the char_key
 *)
let is_char (t: 'a tree) (key: string): bool =
    match t with
    | Leaf -> false
    | Node (c,_,_,_,_) -> key.[0] = c


(**
 * Create an empty new tree
 *)
let create (): 'a tree = Leaf


(**
 * Insert 'a item with a string key and return the updated tree
 * If a value already exists for the string key, replace the stored value
 * returns true if key already exists, key is updated
 * false if key does not exist, key is inserted
 *)
let rec insert (key: string) (item: 'a) (t: 'a tree): (bool * 'a tree) =
    let key = if key = "" then " " else key in
    match t with
    | Leaf ->
        (if (String.length key) = 1 then
            (false, Node (key.[0], Some item, Leaf, Leaf, Leaf))
        else
            let new_key = String.sub key 1 (String.length key - 1) in
            let (flag, new_t) = insert new_key item Leaf in
            (flag, Node (key.[0], None, Leaf, new_t, Leaf)))
    | Node (c, v, t1, t2, t3) ->
        if (key = Char.escaped c) && v = None then
            (false, Node (c, Some item, t1, t2, t3))
        else if (key = Char.escaped c) then
            (true, Node (c, Some item, t1, t2, t3))
        else if (key.[0] = c) then
            let new_key = String.sub key 1 (String.length key - 1) in
            let (flag, new_t) = insert new_key item t2 in
            (flag, Node (c, v, t1, new_t, t3))
        else if (key.[0] < c) then
            let (flag, new_t) = insert key item t1 in
            (flag, Node (c, v, new_t, t2, t3))
        else (* key.[0] > c *)
            let (flag, new_t) = insert key item t3 in
            (flag, Node (c, v, t1, t2, new_t))


(**
 * Remove 'a item with a string key and return the updated tree
 * If string key does not exist, return the original tree
 * return true if key is removed, tree is updated
 * false if key does not exist, tree is unchanged
 *)
let rec remove (key: string) (t: 'a tree): (bool * 'a tree) =
    let key = if key = "" then " " else key in
    match t with
    | Leaf -> (false, Leaf)
    | Node (c, v, t1, t2, t3) when key = Char.escaped c ->
        let flag = v <> None in
        (flag, Node (c, None, t1, t2, t3))
    | Node (c, v, t1, t2, t3) when key.[0] = c ->
        let new_key = String.sub key 1 (String.length key - 1) in
        let (flag, new_t) = remove new_key t2 in
        (flag, Node (c, v, t1, new_t, t3))
    | Node (c, v, t1, t2, t3) ->
        if (key.[0] < c) then
            let (flag, new_t) = remove key t1 in
            (flag, Node (c, v, new_t, t2, t3))
        else (* key.[0] > c *)
            let (flag, new_t) = remove key t3 in
            (flag, Node (c, v, t1, t2, new_t))

(**
 * Search for a string key in the tree and return Some of 'a item
 * If string key does not exist, return None
 *)
let rec get (key: string) (t: 'a tree): 'a option =
    let key = if key = "" then " " else key in
    match t with
    | Leaf -> None
    | Node (c, v, t1, t2, t3) ->
        if (key = Char.escaped c) then v
        else if (key.[0] = c) then
            let new_key = String.sub key 1 (String.length key - 1) in
            get new_key t2
        else if (key.[0] < c) then get key t1
        else get key t3 (* key.[0] > c *)

(* Return string representing an int option *)
let string_int_option = function
    | None -> "None"
    | Some v -> string_of_int v

(* Return string representing an int TST *)
let rec string_int_tst = function
    | Leaf -> "Leaf"
    | Node (c, v, t1, t2, t3) ->
        "Node(\"" ^ (Char.escaped c) ^ "\"," ^ (string_int_option v) ^ ","
        ^ (string_int_tst t1) ^ "," ^ (string_int_tst t2) ^ ","
        ^ (string_int_tst t3) ^ ")"

(**
 * Print an int tree to terminal
 *)
let print_int_tst (t: int tree): unit = print_endline (string_int_tst t)

(* Return string representing an alpha TST *)
let rec string_a_tst = function
    | Leaf -> "Leaf"
    | Node (c, None, t1, t2, t3) ->
        "Node(\"" ^ (Char.escaped c) ^ "\", None,"
        ^ (string_a_tst t1) ^ "," ^ (string_a_tst t2) ^ ","
        ^ (string_a_tst t3) ^ ")"
    | Node (c, _, t1, t2, t3) ->
        "Node(\"" ^ (Char.escaped c) ^ "\", Some,"
        ^ (string_a_tst t1) ^ "," ^ (string_a_tst t2) ^ ","
        ^ (string_a_tst t3) ^ ")"

(**
 * Print keys of an 'a tree to terminal
 *)
let print_a_tst (t: 'a tree): unit = print_endline (string_a_tst t)

(**
 * Return alphabetical list of keys and value options
 *)
let rec keys_tst = function
    | Leaf -> []
    | Node (c, curr_v, t1, t2, t3) ->
        let rec prepend lst =
            match lst with
            | [] -> []
            | (k, prev_v)::t -> (((Char.escaped c) ^ k), prev_v)::(prepend t) in
        let split = if curr_v <> None then [(Char.escaped c, curr_v)] else [] in
        keys_tst t1 @ split @ (prepend (keys_tst t2)) @ keys_tst t3

(**
 * Print alphabetical list of keys and values
 *)
let rec print_keys = function
    | [] -> ()
    | (k, v')::t -> Printf.printf "%s, %s\n" k (string_int_option v'); print_keys t

(**
 * Return alphabetical list of keys and values
 *)
let list_tst t =
    let rec strip_option = function
    | (k, Some x)::xs -> (k, x)::(strip_option xs)
    | _ -> [] in
    strip_option (keys_tst t)
