(**
 * Implementation of a ternary search tree
 * Only unique string paths are allowed
 * If key is empty "", replace key with one whitespace " "
 *)

 type 'a tree

(**
 * Create an empty new tree
 *)
val create: unit -> 'a tree

(**
 * Insert 'a item with a string key and return the updated tree
 * If a value already exists for the string key, replace the stored value
 * returns true if key already exists, key is updated
 * false if key does not exist, key is inserted
 *)
val insert: string -> 'a -> 'a tree -> (bool * 'a tree)

(**
 * Remove 'a item with a string key and return the updated tree
 * If string key does not exist, return the original tree
 * return true if key is removed, tree is updated
 * false if key does not exist, tree is unchanged
 *)
val remove: string -> 'a tree -> (bool * 'a tree)

(**
 * Search for a string key in the tree and return Some of 'a item
 * If string key does not exist, return None
 *)
val get: string -> 'a tree -> 'a option

(**
 * Print an int tree to terminal
 *)
val print_int_tst: int tree -> unit

(**
 * Print keys of an 'a tree to terminal
 *)
val print_a_tst: 'a tree -> unit


(**
 * Return alphabetical list of keys and value options
 *)
val keys_tst: 'a tree -> (string * 'a option) list

(**
 * Return alphabetical list of keys and values
 *)
val list_tst: 'a tree -> (string * 'a) list

(**
 * Print alphabetical list of keys and values
 *)
val print_keys: (string * int option) list -> unit
