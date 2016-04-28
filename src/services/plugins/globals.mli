(** [LintMap] is a Map containing all information about the linter.
    The key is a [string] representing the linter name, and the value of the
    map contains a list of [input] (registered mains). *)
module LintMap : sig
  type 'a t
  type key = string
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val find : key -> 'a t -> 'a
  val cardinal : 'a t -> int
end

(** [Config] is a module which allow to create options for the configuration
    file and command-line arguments. *)
module Config : Configuration.CONFIG

(** [plugins] is a global data structure where all plugins are registered.
    The keys of the structure are a [Plugin_types.PLUGIN] and the value are
    a [LintMap.t]. *)
val plugins :
  ((module Plugin_types.PLUGIN), (Input.input list) LintMap.t) Hashtbl.t
