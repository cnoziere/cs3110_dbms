(**
 * Implementation of a ternary search tree
 * Only unique string paths are allowed
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

let create (): 'a tree = Leaf


let rec insert (key: string) (item: 'a option) (t: 'a tree): 'a tree =
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


let rec remove (key: string) (t: 'a tree): 'a tree =
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
