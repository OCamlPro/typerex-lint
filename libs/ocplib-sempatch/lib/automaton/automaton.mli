type t

val empty : t

val add_state :
  ?final:bool
  -> ?updates_loc:bool
  -> ?replacement_tree:Tree.t
  -> t
  -> (t * State.id)

val get_state : t -> State.id -> State.t option

val add_transition : State_tree.t -> Tree.t -> State.id -> t -> t

val states : t -> State.t list
val transitions : t -> Transitions.t

(* val update_state : State.id -> (State.t -> State.t) -> t -> t *)
val add_replacement : State.id -> Tree.t -> t -> t

(* val run_node1 : t -> Tree.node1 -> (State.t * Tree.node1 Env.t) option *)
