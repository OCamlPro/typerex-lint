(** Values representing the kind of warnings. See [Warning_types.kind] for more
    details. *)
val kind_code : Warning_types.kind
val kind_typo : Warning_types.kind
val kind_interface : Warning_types.kind
val kind_metrics : Warning_types.kind
val new_kind : string -> Warning_types.kind

(** [kind_to_string kind] returns the string representation of
    [Warning_types.kind]. *)
val kind_to_string : Warning_types.kind -> string


(**** Warnings data structure. ****)

(** Abstract type representation the warnings data structure. *)
type t

(** The empty set of warning. *)
val empty : t

(** [add loc id kinds short_name message wset] adds the warning to [wset] with
    the location [loc], warning number [id], kinds [kinds], a short message
    [short_message] which will be display at command line or in configuration
    file and the message [message] which represents the message displayed when
    the warning will be emit.*)
val add :
  Location.t -> int -> Warning_types.kinds -> string -> string -> t -> unit

(** [add_warning warning wset] adds the warning [warning] to [wset]. *)
val add_warning : Warning_types.warning -> t -> unit

(** [iter f wset] applies [f] in turn to all elements of [wset]. The elements of
    [wset] are presented to [f] in increasing order with respect to the ordering
    over the type of elements. *)
val iter : (Warning_types.warning -> unit) -> t -> unit

(** A printing function over the type [Warning_types.warning].  *)
val print : Format.formatter -> Warning_types.warning -> unit
