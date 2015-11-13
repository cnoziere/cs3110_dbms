
(* tree data structure storing values of type 'a *)
type 'a tree

(* string key to uniquely idenity a node in the tree *)
type key = string

(* values in the tree must be of a consistent type 'a *)
type value = 'a

val create : unit -> tree

val lookup : tree -> key -> value option

val member : tree -> key -> bool

val insert : tree -> key -> value -> tree

val remove : tree -> key -> unit
