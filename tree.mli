
(**
 * tree is a mutable tree data structure storing values of type 'a
 *)
type 'a tree

(**
 * t_key is a string that uniquely idenity each node in the tree
 *)
type t_key = string

(**
 * t_value represents values in the tree, which must be of a consistent type 'a
 *)
type t_value = 'a

(**
 * Create a new tree, and return the resulting empty tree
 *)
val create : unit -> tree

(**
 * Returns as an option the value associated with the provided key, given
 * the tree in which to search and the key to search for
 * If the key is not in the tree, return None
 *)
val lookup : tree -> t_key -> t_value option

(**
 * Determine whether the key exists, given the tree and the key
 * Returns true iff the key is in the tree
 *)
val key_member : tree -> t_key -> bool

(**
 * Determine whether the value exists, given the tree and the value
 * Returns true iff the given value is associated with any key in the tree
 *)
val value_member : tree -> t_value -> bool

(**
 * Inserts a (key,value) pair into the tree, given the tree, key, and value
 * If the key already exists, update the key to have the new value
 * Returns unit
 *)
val insert : tree -> t_key -> t_value -> unit

(**
 * Removes the given key from the dictionary, given the tree and key
 * If the key is not present, nothing is changed
 * Returns unit
 *)
val remove : tree -> t_key -> unit
