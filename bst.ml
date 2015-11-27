(**
 * Implementation of a binary search tree
 * Only unique integer keys are allowed
 *)

(* Each node contains an int key, an 'a value, a left and a right subtree *)
type 'a tree =
    | Leaf
    | Node of (int * 'a option * 'a tree * 'a tree)

let create (): 'a tree = Leaf

let rec insert (key: int) (item: 'a) (t: 'a tree): 'a tree =
    match t with
    | Leaf -> Node (key, Some item, Leaf, Leaf)
    | Node (k, v, left, right) ->
        if k = key then Node (k, Some item, left, right)
        else if (key < k) then
            Node (k, v, (insert key item left), right)
        else (* key > k *)
            Node (k, v, left, (insert key item right))


(* TODO: indicate that no item was removed *)

let rec remove (key: int) (t: 'a tree): 'a tree =
    match t with
    | Leaf -> Leaf
    | Node (k, v, left, right) ->
        if k = key then Leaf
        else if (key < k) then
            Node (k, v, (remove key left), right)
        else (* key > k *)
            Node (k, v, left, (remove key right))


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
