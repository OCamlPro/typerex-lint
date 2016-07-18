automaton/module Nodes =
struct
  type node1 = Node11 | Node12
  and node2 = Node21 | Node22
  and leaf1 = string
  and t =
    | Node1 of node1
    | Node2 of node2
    | Leaf1 of leaf1
    [@@deriving ord, show]
end

type node1 =
  | Node11 of node1
  | Node12 of node2 * leaf1
and node2 =
  | Node21 of node1
  | Node22 of leaf1
and leaf1 = string
and t =
  | Node1 of node1
  | Node2 of node2
  | Leaf1 of leaf1
  [@@deriving show]

let to_node =
  let module N = Nodes in
  function
  | Node1 (Node11 _) -> N.Node1 N.Node11
  | Node1 (Node12 _) -> N.Node1 N.Node12
  | Node2 (Node21 _) -> N.Node2 N.Node21
  | Node2 (Node22 _) -> N.Node2 N.Node22
  | Leaf1 i -> N.Leaf1 i

let compare_nodes n1 n2 =Nodes.compare (to_node n1) (to_node n2)
