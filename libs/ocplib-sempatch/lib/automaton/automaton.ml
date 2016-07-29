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

let add_state
    ?(final=false)
    ?(updates_loc=false)
    ?replacement_tree
    t
  =
  let state = State.new_state
      ~final
      ~updates_loc
      ~replacement_tree
      ()
  in
  update_states t (IM.add (S.id state) state t.states), S.id state

let get_state t id =
  try
    Some (IM.find id t.states)
  with
  Not_found -> None

let update_state id transf t =
  match get_state t id with
  | None -> t
  | Some s ->
    let new_state = transf s in
    if State.id s <> State.id new_state then
      Messages.warn "Changing the id of the state while updating\n";
    update_states t (IM.add (S.id new_state) new_state t.states)

let add_replacement id replacement_tree =
  update_state id (State.set_replacement_tree replacement_tree)

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

let states t =
  IM.bindings t.states
  |> List.split
  |> snd

let transitions t = t.transitions

let step t stree tree env =
  let%map current_state =
    go_one_step t stree tree
  in
  current_state, env

(* let rec run_node1 t node = *)
(*   let%bind state_tree, env = *)
(*     match node with *)
(*     | T.Node12 (sub0, sub1) -> *)
(*       let%map state0, env0 = run_node2 t sub0 *)
(*       and state1, env1 = run_leaf1 t sub1 in *)
(*       let env0 = match State.replacement_tree state0 with *)
(*         | Some (T.Node2 n) -> Env.set_replacement n env0 *)
(*         | _ -> *)
(*           env0 *)
(*       and env1 = match State.replacement_tree state1 with *)
(*         | Some (T.Leaf1 l) -> Env.set_replacement l env1 *)
(*         | _ -> env1 *)
(*       in *)
(*       (St.Node1 (St.Node12 (S.id state0, S.id state1))), *)
(*       Env.merge (fun x y -> T.Node12(x, y)) env0 env1 *)
(*     | T.Node11 sub0 -> *)
(*       let%map state0, env0 = run_node1 t sub0 in *)
(*       (St.Node1 (St.Node11 (S.id state0))), *)
(*       Env.map_replacement (fun r -> T.Node11 r) env0 *)
(*   in *)
(*   let%map new_state, env = step t state_tree (T.Node1 node) env in *)
(*   let env = match State.replacement_tree new_state with *)
(*     | Some (T.Node1 n) -> Env.set_replacement n env *)
(*     | _ -> env *)
(*   in new_state, env *)
(*  *)
(* and run_node2 t node = *)
(*   let%bind state_tree, env = *)
(*     match node with *)
(*     | T.Node21 sub0 -> *)
(*       let%map state0, env0 = run_node1 t sub0 in *)
(*       (St.Node2 (St.Node21 (S.id state0))), *)
(*       Env.map_replacement (fun r -> T.Node21 r) env0 *)
(*     | T.Node22 sub0 -> *)
(*       let%map state0, env0 = run_leaf1 t sub0 in *)
(*       (St.Node2 (St.Node22 (S.id state0))), *)
(*       Env.map_replacement (fun r -> T.Node22 r) env0 *)
(*   in *)
(*   step t state_tree (T.Node2 node) env *)
(*  *)
(* and run_leaf1 t leaf = *)
(*   step t St.Unit (T.Leaf1 leaf) (Env.mk_new leaf) *)
