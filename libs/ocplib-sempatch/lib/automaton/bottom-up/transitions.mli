(**
   The set of transitions of an automaton
*)

type t

val empty : t

val add : t -> State_tree.t -> Tree.t -> State.id -> t

val follow : t -> State_tree.t -> Tree.t -> State.id option
