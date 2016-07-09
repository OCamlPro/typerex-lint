(**
   The set of transitions of an automaton
*)

type t

val empty : t

val add : t -> State_tree.t -> Tree.t -> State.id -> t

val follow : t -> State_tree.t -> Tree.t -> State.id option

val iter : (State_tree.t -> Tree.Nodes.t -> State.id -> unit) -> t -> unit
