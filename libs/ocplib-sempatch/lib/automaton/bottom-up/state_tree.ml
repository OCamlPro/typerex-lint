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

let id_string = function
  | Node1 Node11 s -> Printf.sprintf "Node1_Node11_%s" (State.show s)
  | Node1 Node12 (s1, s2) -> Printf.sprintf "Node1_Node11_%s_%s"
                               (State.show s1)
                               (State.show s2)
  | Node2 Node21 s -> Printf.sprintf "Node2_Node21_%s" (State.show s)
  | Node2 Node22 s -> Printf.sprintf "Node2_Node22_%s" (State.show s)
  | Leaf1 s -> Printf.sprintf "Leaf1_%s" (State.show s)
  | Unit -> "Unit"
