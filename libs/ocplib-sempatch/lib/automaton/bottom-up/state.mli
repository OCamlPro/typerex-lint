module Identifier :
sig
  include Map.OrderedType

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

type id = Identifier.t

type t [@@deriving show]

val new_state :
  ?final:bool
  -> ?updates_loc:bool
  -> ?replacement_tree:Tree.t option
  -> unit
  -> t

val is_final : t -> bool
val updates_loc : t -> bool
val replacement_tree : t -> Tree.t option
val id : t -> id

val set_replacement_tree : Tree.t -> t -> t
