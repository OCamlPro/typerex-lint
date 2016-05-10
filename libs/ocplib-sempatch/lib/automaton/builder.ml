module A = Automaton
open Parsetree

let final = A.{
  transitions = [];
  final = true;
}

let make_report s =
  A.{ s with
    transitions = List.map (fun (_, f) -> true, f) s.transitions;
  }


let states_or s1 s2 =
  A.{
    transitions = s1.transitions @ s2.transitions;
    final = s1.final || s2.final;
  }

let match_var var = A.{
  transitions = [
    false,
    fun _self expr ->
      match expr.pexp_desc with
      | Pexp_ident ({ Asttypes.txt = id; _ })
        when id = var ->
        [[final]]
      | _ -> []
  ];
  final = false;
}

and match_const const = A.{
  transitions = [
    false,
    fun _self expr ->
      match expr.pexp_desc with
      | Pexp_constant cst
        when cst = const ->
        [[final]]
      | _ -> []
  ];
  final = false;
}

and match_any = A.{
  transitions = [
    false,
    fun _self _expr -> [[final]]
  ];
  final = false;
}

let match_apply left right = A.{
  transitions = [
    false,
    fun _self expr ->
      match expr with
      | { pexp_desc = Pexp_apply _; _} ->
        [[left; right]]
      | _ -> []
  ];
  final = false;
}

let catchall =
  let list_set n value = List.mapi (fun i elt -> if i=n then value else elt) in
  let rec list_make defaule_value = function
    | 0 -> []
    | n when n > 0 -> defaule_value :: list_make defaule_value (n-1)
    | _ -> raise (Invalid_argument "list_make")
  in
  let sub self n =
    list_make (list_make final n) n
    |> List.mapi (fun idx elt -> list_set idx self elt)
  in
  A.{
    transitions = [
      false,
      fun self expr ->
        match expr with
        | { pexp_desc = Pexp_let _; _ }
        | { pexp_desc = Pexp_apply _; _ } ->
          sub self 2
        | { pexp_desc = Pexp_ident _; _} ->
          []
        | _ -> []
    ];
    final = false;
  }

let trash = A.{
    transitions = [];
    final = false;
  }

let rec build_automaton expression =
  match expression.pexp_desc with
  | Pexp_ident { Asttypes.txt = id; _ } ->
    match_var id
  | Pexp_constant c ->
    match_const c
  | Pexp_apply (f, ["", arg]) ->
    match_apply (build_automaton f) (build_automaton arg)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_inside"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    states_or catchall (build_automaton e)
  | Pexp_extension ({ Asttypes.txt = "__sempatch_report"; _},
                    PStr [{ pstr_desc = Pstr_eval (e, _); _ }]) ->
    (build_automaton e |> make_report)
  | _ -> assert false
