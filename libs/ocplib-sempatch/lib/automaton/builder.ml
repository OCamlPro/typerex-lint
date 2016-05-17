module A = Automaton
module AE = Ast_element
open Parsetree
open Std_utils

let final () = A.{
    transitions = [];
    final = true;
  }

let make_report s =
  A.{ s with
      transitions = List.map (fun (_, f) -> true, f) s.transitions;
    }


let add_elements_from s1 s2 =
  let open A in
  s1.transitions <- s1.transitions @ s2.transitions;
  s1.final <- s1.final || s2.final;
  s1

let ignore_meta f m y = List.map (fun elt -> elt, m) (f y)

let basic_state f = A.{
    transitions = [
      false,
      ignore_meta @@ f
    ];
    final = false;
  }

let match_var var =
  basic_state @@ fun expr ->
  match expr with
  | { pexp_desc = Pexp_ident ({ Asttypes.txt = id; _ }); _ }
    when id = var ->
    [A.Final]
  | _ -> []

and match_const const =
  basic_state @@ fun expr ->
  match expr with
  | { pexp_desc = Pexp_constant cst; _ }
    when cst = const ->
    [A.Final]
  | _ -> []

and match_apply left right =
  basic_state @@ fun expr ->
  match expr with
  | { pexp_desc = Pexp_apply _; _} ->
    [A.(Expr (Apply (left, right)))]
  | _ -> []

and match_ifthenelse eif ethen eelse =
  basic_state @@ fun expr ->
  match expr.pexp_desc with
  | Pexp_ifthenelse _ ->
    [A.(Expr (Ifthenelse (eif, ethen, eelse)))]
  | _ -> []

and match_construct id sub_state =
  basic_state @@ fun expr ->
  match expr with
  | { pexp_desc = Pexp_construct ({ Asttypes.txt = ast_id; _ }, _); _}
    when id = ast_id ->
    [A.(Expr (Construct sub_state))]
  | _ -> []

let match_let isrec left right =
  basic_state @@ fun expr ->
  match expr with
  | { pexp_desc = Pexp_let (rec_flag, _, _); _ } ->
    if rec_flag = isrec then
      [A.(Expr (Let (left, right)))]
    else
      []
  | _ -> []

and match_var_pattern var =
  basic_state @@ fun pat ->
  match pat with
  | { ppat_desc = Ppat_var ({ Asttypes.txt = id; _ }); _ }
    when id = var ->
    [A.Final]
  | _ -> []

and match_value_binding pattern expr =
  basic_state @@ fun _ ->
  [A.(Value_binding { vb_pat = pattern; vb_expr = expr; } ) ]

and match_any_expr id = A.{
    transitions = [
      false,
      fun meta expr ->
        [
          Final,
          {meta with
           Match.substitutions =
             Substitution.add_expr
               id
               expr
               meta.Match.substitutions
          }
        ]
    ];
    final = false;
  }

and match_any_pattern id = A.{
    transitions = [
      false,
      fun meta pat ->
        [
          Final,
          {meta with
           Match.substitutions =
             Substitution.add_pattern
               id
               pat
               meta.Match.substitutions
          }
        ]
    ];
    final = false;
  }

let catchall_expr () =
  let state =
    A.{
      transitions = [];
      final = false;
    }
  in state.A.transitions <-
    A.[
      false,
      ignore_meta @@ fun expr ->
      match expr with
      | { pexp_desc = Pexp_apply _; _ } ->
        [
          Expr (Apply (final (), state));
          Expr (Apply (state, final ()));
        ]
      | { pexp_desc = Pexp_ident _; _} ->
        []
      | _ -> []
    ];
  state

let trash () = A.{
    transitions = [];
    final = false;
  }

let rec from_expr metas expr =
  match expr.pexp_desc with
  | Pexp_ident { Asttypes.txt = Longident.Lident id; _ }
    when List.mem id metas
    ->
    match_any_expr id
  | Pexp_ident { Asttypes.txt = id; _ } ->
    match_var id
  | Pexp_constant c ->
    match_const c
  | Pexp_construct ({ Asttypes.txt = id; _ }, expr_opt) ->
    match_construct id (Option.map (from_expr metas) expr_opt)
  | Pexp_apply (f, ["", arg]) ->
    match_apply (from_expr metas f) (from_expr metas arg)
  | Pexp_let (isrec, bindings, expr) ->
    match_let isrec (from_value_bindings metas bindings) (from_expr metas expr)
  | Pexp_ifthenelse (eif, ethen, eelse) ->
    match_ifthenelse
      (from_expr metas eif)
      (from_expr metas ethen)
      (Option.map (from_expr metas) eelse)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_inside"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    add_elements_from (catchall_expr ()) (from_expr metas e)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_report"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    (from_expr metas e |> make_report)
  | _ -> assert false

and from_pattern metas pattern =
  match pattern.ppat_desc with
  | Ppat_var { Asttypes.txt = id; _ }
    when List.mem id metas
    ->
    match_any_pattern id
  | Ppat_var { Asttypes.txt = id; _ } ->
    match_var_pattern id
  | _ -> assert false

and from_value_binding metas { pvb_expr; pvb_pat; _ } =
  match_value_binding
    (from_pattern metas pvb_pat)
    (from_expr metas pvb_expr)

and from_value_bindings metas =
  List.map (from_value_binding metas)
