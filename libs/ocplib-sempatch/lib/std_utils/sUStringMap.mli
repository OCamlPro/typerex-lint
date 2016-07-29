include Map.S with type key = string
val from_list_pair : (string * 'a) list -> 'a t
val get : string -> 'a t -> 'a option
