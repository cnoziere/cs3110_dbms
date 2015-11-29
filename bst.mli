type 'a tree

(**
 * Create an empty new tree
 *)
val create: unit -> 'a tree

(**
 * Insert 'a item with an int key and return the updated tree
 * If a value already exists for the int key, replace the stored value
 * returns true if key already exists, key is updated
 * false if key does not exist, key is inserted
 *)
val insert: int -> 'a -> 'a tree -> (bool * 'a tree)

(**
 * Remove 'a item with a int key and return the updated tree
 * If int key does not exist, return the original tree
 * return true if key is removed, tree is updated
 * false if key does not exist, tree is unchanged
 *)
val remove: int -> 'a tree -> (bool * 'a tree)

(**
 * Search for a int key in the tree and return Some of 'a item
 * If int key does not exist, return None
 *)
val get: int -> 'a tree -> 'a option

(**
 * Return list of keys and values
 *)
val list_bst: 'a tree -> (int * 'a) list

(**
 * Print a string tree to terminal
 *)
val print_string_bst: string tree -> unit
