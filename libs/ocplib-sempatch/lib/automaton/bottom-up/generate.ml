open Std_utils

open Option.Infix

module T = Tree
module St = State_tree
module A = Automaton

let rec generate automaton parent_state tree = match tree with
  | T.Node1 (T.Node11 sub0) ->
    let (automaton, state0) = A.add_state automaton in
    let automaton =
      A.add_transition
        St.(Node1 (Node11 state0))
        tree
        parent_state
        automaton
    in
    generate automaton state0 (T.Node1 sub0)
  | T.Node1 (T.Node12 (sub0, sub1)) ->
    let automaton, state0 = A.add_state automaton in
    let automaton, state1 = A.add_state automaton in
    let automaton =
      A.add_transition
        St.(Node1 (Node12 (state0, state1)))
        tree
        parent_state
        automaton
    in
    let automaton = generate automaton state0 (T.Node2 sub0) in
    generate automaton state1 (T.Leaf1 sub1)
  | T.Node2 (T.Node21 sub0) ->
    let (automaton, state0) = A.add_state automaton in
    let automaton =
      A.add_transition
        St.(Node2 (Node21 state0))
        tree
        parent_state
        automaton
    in
    generate automaton state0 (T.Node1 sub0)
  | T.Node2 (T.Node22 sub0) ->
    let (automaton, state0) = A.add_state automaton in
    let automaton =
      A.add_transition
        St.(Node2 (Node22 state0))
        tree
        parent_state
        automaton
    in
    generate automaton state0 (T.Leaf1 sub0)
  | T.Leaf1 _ ->
    let automaton =
      A.add_transition
        St.Unit
        tree
        parent_state
        automaton
    in
    automaton

let from_tree tree =
  let automaton = Automaton.empty in
  let (automaton, top_state) = Automaton.add_state ~final:true automaton in
  generate automaton top_state tree

let () =
  let tree = T.(Node1 (Node11 (Node11 (Node12 (Node22 "foo", "bar"))))) in
  let automaton = from_tree tree in
  ignore @@
  let%map final_state, _ = Automaton.run automaton tree () in
  Messages.debug "Final state : %s\n" @@ State.show final_state
