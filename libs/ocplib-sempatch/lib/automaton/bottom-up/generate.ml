open Std_utils

open Option.Infix

module T = Tree
module St = State_tree
module A = Automaton

let rec generate_node1 automaton parent_state tree = match tree with
  | T.Node11 sub0 ->
    let (automaton, state0) = A.add_state automaton in
    let automaton =
      A.add_transition
        St.(Node1 (Node11 state0))
        (T.Node1 tree)
        parent_state
        automaton
    in
    generate_node1 automaton state0 sub0
  | T.Node12 (sub0, sub1) ->
    let automaton, state0 = A.add_state
        ~replacement_tree:Tree.(Node2 (Node21 (Node12 (Node22 "blih", "bleh"))))
        automaton
    in
    let automaton, state1 = A.add_state automaton in
    let automaton =
      A.add_transition
        St.(Node1 (Node12 (state0, state1)))
        (T.Node1 tree)
        parent_state
        automaton
    in
    let automaton = generate_node2 automaton state0 sub0 in
    generate_leaf1 automaton state1 sub1


and generate_node2 automaton parent_state tree = match tree with
  | T.Node21 sub0 ->
    let (automaton, state0) = A.add_state automaton in
    let automaton =
      A.add_transition
        St.(Node2 (Node21 state0))
        (T.Node2 tree)
        parent_state
        automaton
    in
    generate_node1 automaton state0 sub0
  | T.Node22 sub0 ->
    let (automaton, state0) = A.add_state
        automaton
    in
    let automaton =
      A.add_transition
        St.(Node2 (Node22 state0))
        T.(Node2 tree)
        parent_state
        automaton
    in
    generate_leaf1 automaton state0 sub0

and generate_leaf1 automaton parent_state tree =
  A.add_transition
    St.Unit
    (T.Leaf1 tree)
    parent_state
    automaton

let from_node1 tree =
  let automaton = Automaton.empty in
  let (automaton, top_state) = Automaton.add_state ~final:true automaton in
  generate_node1 automaton top_state tree

let () =
  let tree = T.((Node11 (Node11 (Node12 (Node22 "foo", "bar"))))) in
  let automaton = from_node1 tree in
  ignore @@
  let%map final_state, env = Automaton.run_node1 automaton tree in
  Messages.debug "Final state : %s\n" @@ State.show final_state;
  Messages.debug "Env : %s\n" @@ Env.show Tree.pp_node1 env;
  Export.toDot stdout automaton
