(**
 * Implementation of a binary search tree
 * Only unique integer keys are allowed
 *)

exception Duplicate (* of string *)

(* Each node contains an int key, an 'a value, a left and a right subtree *)
type 'a tree =
    | Leaf
    | Node of (int * 'a option * 'a tree * 'a tree)

(* Return the key and value belonging to the minimum key in the tree
(int * 'a option) option *)
let rec get_min = function
    | Leaf -> None
    | Node (k, v, left, right) ->
        if left = Leaf then Some (k, v)
        else get_min left

(* check if tree is a valid BST *)
let rec is_valid = function
    | Leaf -> true
    | Node (k, v, left, right) ->
        let check_left = match left with
            | Leaf -> true
            | Node(left_k,_,_,_) -> left_k < k in
        let check_right = match right with
            | Leaf -> true
            | Node(right_k,_,_,_) -> right_k > k in
        check_left && check_right && is_valid left && is_valid right


let create (): 'a tree = Leaf


(* true if key already exists, key is updated
 * false if key does not exist, key is inserted *)
let rec insert (key: int) (item: 'a) (t: 'a tree): (bool * 'a tree) =
    match t with
    | Leaf -> (false, Node (key, Some item, Leaf, Leaf))
    | Node (k, v, left, right) ->
        if k = key then (true, Node (k, Some item, left, right))
        else if (key < k) then
            let (flag, l) = insert key item left in
            (flag, Node (k, v, l, right))
        else (* key > k *)
            let (flag, r) = insert key item right in
            (flag, Node (k, v, left, r))


(* true if key is removed, tree is updated
 * false if key does not exist, tree is unchanged *)
let rec remove (key: int) (t: 'a tree): (bool * 'a tree) =
    match t with
    | Leaf -> (false, Leaf)
    | Node (k, v, left, right) when k = key ->
        (if (left = Leaf && right = Leaf) then (true, Leaf)
        else if left = Leaf then (true, right)
        else if right = Leaf then (true, left)
        else
            match get_min right with
            | None -> failwith "Case not possible"
            | Some (min_key, min_value) ->
                let (_, r) = remove min_key right in
                (true, Node (min_key, min_value, left, r)))
    | Node (k, v, left, right) ->
        if (key < k) then
            let (flag, l) = remove key left in
            (flag, Node (k, v, l, right))
        else (* key > k *)
            let (flag, r) = remove key right in
            (flag, Node (k, v, left, r))


let rec get (key: int) (t: 'a tree): 'a option =
    match t with
    | Leaf -> None
    | Node (k, v, left, right) ->
        if k = key then v
        else if (key < k) then
            get key left
        else (* key > k *)
            get key right


let print_string_bst (t: string tree): unit =
    let string_option = function
        | None -> "None"
        | Some v -> v in
    let rec string_bst = function
        | Leaf -> "Leaf"
        | Node (k, v, left, right) ->
            "Node(\"" ^ string_of_int k ^ "\"," ^ (string_option v) ^ ","
            ^ (string_bst left) ^ "," ^ (string_bst right) ^ ")" in
    print_endline (string_bst t)
