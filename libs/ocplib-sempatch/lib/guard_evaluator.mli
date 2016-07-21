val eval : Substitution.t -> Guard.t -> bool

exception Undefined_var of string
exception Undefined_function of string
exception TypeError

val eval_union : Substitution.t -> Guard.t list -> bool
