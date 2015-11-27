type 'a tree

(**
 * Create an empty new tree
 *)
val create: unit -> 'a tree

(**
 * Insert 'a item with a string and return the updated tree
 * If a value already exists for the string, replace the stored value
 *)
val insert: string -> 'a option -> 'a tree -> 'a tree

(**
 * Remove 'a item with a string key and return the updated tree
 * If string key does not exist, return the original tree
 *)
val remove: string -> 'a tree -> 'a tree

(**
 * Search for a string key in the tree and return Some of 'a item
 * If string key does not exist, return None
 *)
val get: string -> 'a tree -> 'a option

(**
 * Print an int tree to terminal
 *)
val print_int_tst: int tree -> unit
