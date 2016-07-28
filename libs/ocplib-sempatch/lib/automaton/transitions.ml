open Std_utils

module Preimage: (Map.OrderedType with type t = State_tree.t * Tree.Nodes.t) =
struct
  [@@@ocaml.warning "-39"] (* ppx_deriving generates a recursive function,
                              which in this case is useless *)
  type t = State_tree.t * Tree.Nodes.t
    (* [@@deriving ord] *)
  let compare = compare
end

module TransMap = Map.Make(Preimage)

type t = State.id TransMap.t

let empty : t = TransMap.empty

let add transMap state_tree tree dest =
  if TransMap.mem (state_tree, Tree.to_node tree) transMap then
    Messages.debug "Replacing existing transition\n";
  TransMap.add (state_tree, Tree.to_node tree) dest transMap

let follow map orig_states current_node =
  let node = Tree.to_node current_node in
  try
    let new_id = TransMap.find (orig_states, node) map
    in
    (* Messages.debug "Transition from states %s and node %s to state %s\n" *)
    (*   (State_tree.show orig_states) *)
    (*   (Tree.Nodes.show node) *)
    (*   (State.Identifier.show new_id); *)
    Some new_id
  with Not_found ->
    (* Messages.debug "No transition from states %s with node %s\n" *)
    (*   (State_tree.show orig_states) *)
    (*   (Tree.Nodes.show node); *)
    None

let iter f =
  let f' = fun (x,y) -> f x y in
  TransMap.iter f'
