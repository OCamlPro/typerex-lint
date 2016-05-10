type t = {
  transitions : (bool * (t -> Parsetree.expression -> t list list)) list;
  (* The boolean indicates whether the transition starts an expression to be reported *)
  final: bool;
  (* Whether this state is a final state *)
}
