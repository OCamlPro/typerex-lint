type meta_info = Match.t

type expr_states =
  | Apply of Parsetree.expression t * Parsetree.expression t
  | Let of (Parsetree.value_binding) t list * Parsetree.expression t
  | Ifthenelse of Parsetree.expression t * Parsetree.expression t * Parsetree.expression t option
  | Construct of Parsetree.expression t option

and val_binding = {
  vb_pat: Parsetree.pattern t;
  vb_expr : Parsetree.expression t;
}

and pattern

and _ state_bundle =
  | Expr : expr_states -> Parsetree.expression state_bundle
  | Value_binding : val_binding -> Parsetree.value_binding state_bundle
  | Pattern : pattern -> Parsetree.pattern state_bundle
  | Final : 'a state_bundle

and 'a transition =
  (bool *
   ('a t -> meta_info -> 'a  -> ('a state_bundle * meta_info) list)
  )

and 'a t = {
  (* The boolean indicates whether the transition starts an expression
     to be reported *)
  transitions : 'a transition list;
  (* Whether this state is a final state *)
  final: bool;
}
