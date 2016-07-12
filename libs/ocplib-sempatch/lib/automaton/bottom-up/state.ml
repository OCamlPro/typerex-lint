module Identifier :
sig
  include Map.OrderedType with type t = int

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
=
struct
  type t = int [@@deriving show]
  let compare : t -> t -> int = compare
end

type id = Identifier.t

(* This certainly would gain from being parametric in the type of the
   replacement, but it would certainly need to be GADTized to be useful, and
   I'm too lazy to do it now *)
type t = {
  identifier: Identifier.t;
  final: bool;
  updates_loc: bool;
  replacement_tree: Tree.t option;
}
  [@@deriving show]

let global_counter = ref 0

let new_id () =
  let id = !global_counter in
  incr global_counter; id

let new_state
  ?(final=false)
  ?(updates_loc=false)
  ?(replacement_tree=None)
  ()
  =
  {
    identifier = new_id ();
    final;
    updates_loc;
    replacement_tree;
  }

let is_final t = t.final
let updates_loc t = t.updates_loc
let replacement_tree t = t.replacement_tree
let id t = t.identifier

let set_replacement_tree new_tree t =
  { t with replacement_tree = Some new_tree }
