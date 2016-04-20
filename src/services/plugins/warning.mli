
val kind_code : Warning_types.kind
val kind_typo : Warning_types.kind
val kind_interface : Warning_types.kind
val kind_metrics : Warning_types.kind
val new_kind : string -> Warning_types.kind

val kind_to_string : Warning_types.kind -> string

type t

val empty: t

val add: Location.t -> int -> Warning_types.kinds -> string -> string -> t -> unit

val add_warning : Warning_types.warning -> t -> unit

val iter: (Warning_types.warning -> unit) -> t -> unit

val print: Format.formatter -> Warning_types.warning -> unit
