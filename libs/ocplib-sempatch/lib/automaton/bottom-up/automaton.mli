type t

val empty : t

(** Fails if a state with the same id already exists *)
val add_state : State.t -> t -> t

(** Fails if no state with the same id already exists *)
val update_state : State.t -> t -> t

val get_state : t -> State.id -> State.t option

val add_transition : State_tree.t -> Tree.t -> State.id -> t -> t

val run : t -> Tree.t -> Env.t -> (State.t * Env.t) option
