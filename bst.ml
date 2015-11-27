(**
 * Implementation of a binary search tree
 * Only unique integer keys are allowed
 *)

(* Each node contains an int key, an 'a value, a left and a right subtree *)
type 'a tree =
    | Leaf
    | Node of (int * 'a option * 'a tree * 'a tree)

let create (): 'a tree = Leaf

let rec insert (key: int) (item: 'a option) (t: 'a tree): 'a tree =
    match t with
    | Leaf -> Node (key, item, Leaf, Leaf)
    | Node (k, v, left, right) ->
        if k = key then Node (k, item, left, right)
        else if (key < k) then
            Node (k, v, (insert key item left), right)
        else (* key > k *)
            Node (k, v, left, (insert key item right))


let rec remove (key: string) (t: 'a tree): 'a tree =
    match t with
    | Leaf -> Leaf
    | Node (k, v, left, right) ->
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
 * Search for a string key in the tree and return Some of 'a item
 * If string key does not exist, return None
 *)
let rec get (key: string) (t: 'a tree): 'a option =
    match t with
    | Leaf -> None
    | Node (c, v, t1, t2, t3) ->
        if key = "" then failwith "Error: empty keys are not allowed"
        else if (key = Char.escaped c) then v
        else if (key.[0] = c) then
            let new_key = String.sub key 1 (String.length key - 1) in
            if is_char t1 new_key then get new_key t1
            else if is_char t2 new_key then get new_key t2
            else get new_key t3
        else if (key.[0] < c) then get key t1
        else get key t3 (* key.[0] > c *)

(**
 * Print tree to terminal for testing
 *)
let print_int_tst (t: int tree): unit =
    let string_int_option = function
        | None -> "None"
        | Some v -> string_of_int v in
    let rec string_int_tst = function
        | Leaf -> "Leaf"
        | Node (c, v, t1, t2, t3) ->
            "Node(\"" ^ (Char.escaped c) ^ "\"," ^ (string_int_option v) ^ ","
            ^ (string_int_tst t1) ^ "," ^ (string_int_tst t2) ^ ","
            ^ (string_int_tst t3) ^ ")" in
    print_endline (string_int_tst t)
