(*
let printDotArrow ?params out_channel orig dest =
  match params with
  | None ->
    Printf.fprintf out_channel "%s -> %s\n"
      orig dest
  | Some p ->
    Printf.fprintf out_channel "%s -> %s [%s]\n"
      orig dest p

let toDotNodes out_channel nodes =
  List.iter
    (fun state -> Printf.fprintf out_channel "%s [label=\"%s%s\"]\n"
        (State.(Identifier.show @@ id state))
        (State.(Identifier.show @@ id state))
        (match State.replacement_tree state with
         | None -> ""
         | Some t -> "\\nreplacement : " ^ (String.escaped @@ Tree.show t))
    )
    nodes

let toDotStateTree out_channel stree =
  let open State_tree in
  let params = "arrowhead=none" in
  match stree with
  | Node1 (Node11 s)
  | Node2 (Node21 s)
  | Leaf1 s
  | Node2 (Node22 s) ->
    printDotArrow out_channel
        (State.show @@ s)
        (State_tree.id_string stree)
        ~params
  | Node1 (Node12 (s1, s2)) ->
    printDotArrow out_channel
      (State.show @@ s1)
      (State_tree.id_string stree)
      ~params:(params ^ ",label=Node2")
      ;
    printDotArrow out_channel
      (State.show @@ s2)
      (State_tree.id_string stree)
      ~params:(params ^ ",label=Leaf1")
  | Unit -> ()

let toDotArrows out_channel transitions =
  Transitions.iter
    (fun orig_states node dest ->
       let stree_id = State_tree.id_string orig_states in
       Printf.fprintf out_channel
         "%s [shape=circle,width=.01,height=.01,label=\"\"]\n"
         stree_id;
       printDotArrow out_channel
        stree_id
        (State.Identifier.show dest)
        ~params:("label=\"" ^ (String.escaped @@ Tree.Nodes.show node) ^ "\"");
      toDotStateTree out_channel orig_states
    )
    transitions

let toDot out_channel automaton =
  output_string out_channel "digraph automaton {\n";
  toDotNodes out_channel (Automaton.states automaton);
  toDotArrows out_channel (Automaton.transitions automaton);
  output_string out_channel "}\n"
   *)
