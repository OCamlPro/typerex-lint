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

val run : t -> Tree.t -> Env.t -> (State.t * Env.t) option
