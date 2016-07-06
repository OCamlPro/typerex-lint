open! Std_utils
module IM = Map.Make(State.Identifier)
module T = Tree
module St = State_tree
module S = State

open Std_utils.Option.Infix

type t = {
  transitions: Transitions.t;
  states: State.t IM.t;
}

let update_states t states = { t with states = states }
let update_transitions t transitions = { t with transitions = transitions }

let empty = {
  transitions = Transitions.empty;
  states = IM.empty;
}

let maybe_add_state has_to_be_absent state t =
  let old_states = t.states in
  if IM.mem (State.id state) old_states = has_to_be_absent then
    failwith "trying to add a state which was already there"
  else
    update_states t (IM.add (State.id state) state old_states)

let add_state = maybe_add_state true
let update_state = maybe_add_state false

let get_state t id =
  try
    Some (IM.find id t.states)
  with
  Not_found -> None

let add_transition stree tree dest_id t =
  update_transitions t (Transitions.add t.transitions stree tree dest_id)

let go_one_step t state_tree tree =
  let%bind current_state_id =
    Transitions.follow
      t.transitions
      state_tree
      tree
  in
  try Some (IM.find current_state_id t.states)
  with Not_found -> None

let rec run t tree env =
  let res_opt =
    match tree with
    | T.Node1 (T.Node12 (sub0, sub1)) ->
      let%bind state0, env0 = run t (T.Node2 sub0) env in
      let%bind state1, env1 = run t (T.Leaf1 sub1) env in
      let%map current_state =
        go_one_step t (St.Node1 (St.Node12 (S.id state0, S.id state1))) tree
      in
      (current_state, Env.merge env0 env1)
    | T.Node1 (T.Node11 sub0) ->
      let%bind state0, env0 = run t (T.Node1 sub0) env in
      let%map current_state =
        go_one_step t (St.Node1 (St.Node11 (S.id state0))) tree
      in
      (current_state, env0)
    | T.Node2 (T.Node21 sub0) ->
      let%bind state0, env0 = run t (T.Node1 sub0) env in
      let%map current_state =
        go_one_step t (St.Node2 (St.Node21 (S.id state0))) tree
      in
      (current_state, env0)
    | T.Node2 (T.Node22 sub0) ->
      let%bind state0, env0 = run t (T.Leaf1 sub0) env in
      let%map current_state =
        go_one_step t (St.Node2 (St.Node22 (S.id state0))) tree
      in
      (current_state, env0)
    | T.Leaf1 _ ->
      let%map current_state =
        go_one_step t St.Unit tree
      in
      (current_state, env)
  in res_opt

let id = S.id

let () =
  let tree = T.(Node1 (Node11 (Node12 (Node22 "foo", "bar"))))
  and state0 = State.new_state ()
  and state1 = State.new_state ()
  and state2 = State.new_state ()
  and state3 = State.new_state ()
  and state4 = State.new_state ()
  and state5 = State.new_state ()
  in
  let automaton =
    empty
    |> add_state state0
    |> add_state state1
    |> add_state state2
    |> add_state state3
    |> add_state state4
    |> add_state state5
    |> add_transition St.Unit (T.Leaf1 "foo") (id state0)
    |> add_transition St.Unit (T.Leaf1 "bar") (id state1)
    |> add_transition (St.Node2 (St.Node22 (id state0))) T.(Node2 (Node22 "foo")) (id state2)
    |> add_transition (St.Node1 (St.Node12 (id state2, id state1))) T.(Node1 (Node12 (Node22 "foo", "baz"))) (id state3)
    |> add_transition (State_tree.Node1 (State_tree.Node11 (id state3))) tree (id state4)
  in
  ignore @@
  let%map final_state, _ = run automaton tree () in
  Messages.debug "Final state : %s\n" @@ State.show final_state

