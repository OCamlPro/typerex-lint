module StringMap : Map.S with type key = string

type source = Cache | Analyse

type warning_list =
  source * (string list * string) list * Warning_types.warning list
type linter_map = warning_list StringMap.t
type plugin_map = linter_map StringMap.t
type file_map = Digest.t * plugin_map
type t = (string, file_map) Hashtbl.t


module type DATABASE_IO = sig
  val load : string -> t
  val save : string -> t -> unit
end

module type DATABASE = sig
  val init : File.t -> unit
  val load : string -> t
  val save : unit -> unit
  val reset : unit -> unit
  val print : Format.formatter -> unit
  val print_only_new : Format.formatter -> unit
  val print_debug : unit -> unit
  val remove_entry : string -> unit
  val add_entry : string -> string -> string -> unit
  val clean : string list -> unit
  val update : string -> string -> Warning_types.warning -> unit
  val already_run : string -> string -> string -> bool
  val has_warning : unit -> bool
end

module DefaultDB : DATABASE
