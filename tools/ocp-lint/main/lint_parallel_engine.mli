type proc_status = Waiting | Running of int | Done

val waiting_file : unit -> bool

val done_files : int -> int

val find_next_waiting : unit -> string

val mark_waiting : string list -> unit

val mark_running : string -> int -> unit

val mark_done : int -> unit

val get_start_list : int -> string list -> string list
