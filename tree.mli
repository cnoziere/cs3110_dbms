
(* Mutable tree data structure storing values of type 'a *)
type 'a tree

(* String key to uniquely idenity a node in the tree *)
type key = string

(* Values in the tree must be of a consistent type 'a *)
type value = 'a

(* Create a new tree *)
val create : unit -> tree

(* Returns as an option the value associated with the provided key. If
 * the key is not in the tree, return None *)
val lookup : tree -> key -> value option

(* Returns true iff the key exists in the tree *)
val key_member : tree -> key -> bool

(* Returns true iff the given value is associated with any key in the tree *)
val value_member : tree -> value -> bool

(* Inserts a (key,value) pair into the tree. If the key is already, update
 * the key to have the new value *)
val insert : tree -> key -> value -> unit

(* Removes the given key from the dictionary; if the key is not present,
 * nothing is changed *)
val remove : tree -> key -> unit
