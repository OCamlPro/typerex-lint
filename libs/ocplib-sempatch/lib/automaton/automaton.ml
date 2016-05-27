type meta_info = Match.t

type transition =
  bool * (meta_info -> Ast_element.t -> (t list * meta_info) list)

and t = {
  (* The boolean indicates whether the transition starts an expression
     to be reported *)
  mutable transitions : transition list;
  (* Whether this state is a final state *)
  mutable final: bool;
}
