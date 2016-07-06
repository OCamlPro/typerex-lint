module State = State.Identifier

type node1 =
  | Node11 of State.t
  | Node12 of State.t * State.t
and node2 =
  | Node21 of State.t
  | Node22 of State.t
and leaf1 = State.t
and t =
  | Node1 of node1
  | Node2 of node2
  | Leaf1 of leaf1
  | Unit
  [@@deriving ord, show]
