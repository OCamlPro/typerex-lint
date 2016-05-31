open! Tree.Automaton

let ignore_meta f m y = List.map (fun elt -> elt, m) (f y)

let basic_state f = {
    transitions = [
      false,
      ignore_meta @@ f
    ];
    final = false;
  }

let trash = {
    transitions = [];
    final = false;
  }

(* [%%build_automaton_matchers] *)

(* [%%build_automaton_froms] *)
