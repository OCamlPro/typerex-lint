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

let match_expr f = basic_state @@ function
  | AE.Expression e -> f e
  | _ -> []

let match_pat f = basic_state @@ function
  | AE.Pattern e -> f e
  | _ -> []

let match_var var =
  match_expr @@ function
  | { pexp_desc = Pexp_ident ({ Asttypes.txt = id; _ }); _ }
    when id = var ->
    [[final ()]]
  | _ -> []

and match_const const =
  match_expr @@ function
  | { pexp_desc = Pexp_constant cst; _ }
    when cst = const ->
    [[final ()]]
  | _ -> []

and match_apply left right =
  match_expr @@ function
  | { pexp_desc = Pexp_apply _; _} ->
    [[left; right]]
  | _ -> []

and match_ifthenelse eif ethen eelse =
  match_expr @@ function
  | { pexp_desc = Pexp_ifthenelse _; _ } ->
    [[eif; ethen; eelse]]
  | _ -> []

and match_construct id sub_state =
  match_expr @@ function
  | { pexp_desc = Pexp_construct ({ Asttypes.txt = ast_id; _ }, _); _}
    when id = ast_id ->
    [[sub_state]]
  | _ -> []

let match_let isrec bindings_states expr_state =
  match_expr @@ function
  | { pexp_desc = Pexp_let (rec_flag, _, _); _ } ->
    if rec_flag = isrec then
      [[expr_state; bindings_states]]
    else
      []
  | _ -> []

let match_fun lbl_ref default_arg_s arg_s body_s =
  match_expr @@ function
  | { pexp_desc = Pexp_fun (lbl, _, _, _); _ }
    when lbl = lbl_ref ->
    [[default_arg_s; arg_s; body_s]]
  | _ -> []

and match_var_pattern var =
  match_pat @@ function
  | { ppat_desc = Ppat_var ({ Asttypes.txt = id; _ }); _ }
    when id = var ->
    [[final ()]]
  | _ -> []

and match_pat_construct id_ref sub_pat_state =
  match_pat @@ function
  | { ppat_desc = Ppat_construct ({Asttypes.txt = id; _ }, _); _ }
    when id_ref = id ->
    [[sub_pat_state]]
  | _ -> []

and match_value_binding pattern expr =
  basic_state @@ fun _ ->
  [[pattern; expr]]

and match_any_expr id = A.{
    transitions = [
      false,
      fun meta ast_elt ->
        match ast_elt with
        | AE.Expression expr ->
        [
          [final ()],
          {meta with
           Match.substitutions =
             Substitution.add_expr
               id
               expr
               meta.Match.substitutions
          }
        ]
        | _ -> []
    ];
    final = false;
  }

and match_any_pattern id = A.{
    transitions = [
      false,
      fun meta ast_elt ->
        match ast_elt with
        | AE.Pattern pat ->
        [
          [final ()],
          {meta with
           Match.substitutions =
             Substitution.add_pattern
               id
               pat
               meta.Match.substitutions
          }
        ]
        | _ -> []
    ];
    final = false;
  }

let catchall () =
  let open A in
  let state =
    {
      transitions = [];
      final = false;
    }
  in
  let dispatch_list length =
    let rec list_make = function
      | 0 -> []
      | n when n > 0 -> () :: list_make (n-1)
      | _ -> raise (Invalid_argument "list_make")
    in
    let l = list_make length
    in
    List.mapi (fun idx _ ->
        List.mapi (fun jdx _ ->
            if idx = jdx then state else final ()
          )
          l
      )
      l
  in
  let catchall_expressions = function
    | Pexp_construct _
    | Pexp_function _
      ->
       dispatch_list 1

    | Pexp_let _
    | Pexp_apply _
    | Pexp_sequence _
      -> dispatch_list 2

    | Pexp_ifthenelse _
    | Pexp_fun _
      -> dispatch_list 3

    | _ -> []
  and catchall_str_items = function
    | Pstr_eval _
    | Pstr_value _ ->
       dispatch_list 1
    | _ -> []

  and catchall_patterns = function
    | Ppat_construct _
        -> dispatch_list 1
    | _ -> []
  in
    state.transitions <-
    [
      false,
      ignore_meta @@ function
      | AE.Expression e -> catchall_expressions e.pexp_desc
      | AE.Structure_item i -> catchall_str_items i.pstr_desc
      | AE.Pattern p -> catchall_patterns p.ppat_desc
      | AE.Pattern_opt _
      | AE.Expression_opt _
        -> dispatch_list 1
      | AE.Structure _
      | AE.Value_bindings _
      | AE.Value_binding _
      | AE.Cases _
        -> dispatch_list 2
      | AE.Case _
        -> dispatch_list 3
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
    match_construct id (from_maybe_expr metas expr_opt)
  | Pexp_apply (f, ["", arg]) ->
    match_apply (from_expr metas f) (from_expr metas arg)
  | Pexp_let (isrec, bindings, expr) ->
    match_let isrec (from_value_bindings metas bindings) (from_expr metas expr)
  | Pexp_ifthenelse (eif, ethen, eelse) ->
    match_ifthenelse
      (from_expr metas eif)
      (from_expr metas ethen)
      (from_maybe_expr metas eelse)
  | Pexp_fun (lbl, default_arg, arg, body) ->
     match_fun
       lbl
       (from_maybe_expr metas default_arg)
       (from_pattern metas arg)
       (from_expr metas body)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_inside"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    add_elements_from (catchall ()) (from_expr metas e)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_report"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    (from_expr metas e |> make_report)
  | _ -> assert false

and from_maybe_expr metas = function
  | None ->
    begin
      basic_state @@ function
      | AE.Expression_opt None -> [[final ()]]
      | _ -> []
    end
  | Some expr ->
    begin
      let next_state = [[from_expr metas expr]] in
      basic_state @@ function
      | AE.Expression_opt (Some _) -> next_state
      | _ -> []
    end

and from_pattern metas pattern =
  match pattern.ppat_desc with
  | Ppat_var { Asttypes.txt = id; _ }
    when List.mem id metas
    ->
    match_any_pattern id
  | Ppat_var { Asttypes.txt = id; _ } ->
    match_var_pattern id
  | Ppat_construct ({ Asttypes.txt = constr; _ }, sub_pat_opt)
    ->
    match_pat_construct constr (from_pattern_opt metas sub_pat_opt)
  | _ -> assert false

and from_pattern_opt metas = function
  | None ->
    begin
      basic_state @@ function
      | AE.Pattern_opt None -> [[final ()]]
      | _ -> []
    end
  | Some expr ->
    begin
      let next_state = [[from_pattern metas expr]] in
      basic_state @@ function
      | AE.Pattern_opt (Some _) -> next_state
      | _ -> []
    end

and from_value_binding metas { pvb_expr; pvb_pat; _ } =
  match_value_binding
    (from_pattern metas pvb_pat)
    (from_expr metas pvb_expr)

and from_value_bindings metas vbs =
  let aux accu vb =
    basic_state @@ fun _ -> [[from_value_binding metas vb; accu]]
  and terminal = basic_state @@ fun _ -> [[final ()]]
  in
  List.fold_left aux terminal vbs
