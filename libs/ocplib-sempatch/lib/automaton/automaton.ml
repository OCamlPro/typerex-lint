type meta_info = Match.t

type expr_states =
  | Apply of t * t
  | Let of t list * t
  | Ifthenelse of t * t * t option
  | Construct of t option

and val_binding = {
  vb_pat: t;
  vb_expr : t;
}

and pattern

and state_bundle =
  | Expr of expr_states
  | Value_binding of val_binding
  | Pattern of pattern
  | Final

and transition =
  bool * (meta_info -> Ast_element.t -> (state_bundle * meta_info) list)

and t = {
  (* The boolean indicates whether the transition starts an expression
     to be reported *)
  mutable transitions : transition list;
  (* Whether this state is a final state *)
  mutable final: bool;
}
