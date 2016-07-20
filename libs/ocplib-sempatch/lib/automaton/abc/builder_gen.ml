open Std_utils

module H = Ast_helper
module L = Location
module LI = Longident
module C = Abc_common
module Pfx = Abc_common.Pfx

module Arg : Generator.ARG with type result = Parsetree.structure =
struct
  type result = Parsetree.structure
  type middle = string * Parsetree.expression

  let name = "Builder"

  let gen_pattern_constr_args args =
    let convert_fun = Fun.compose H.Pat.var L.mknoloc [@ocaml.warning "-48"] in
    match C.gen_args args with
    | [] -> None
    | [elt] -> Some (convert_fun elt)
    | lst -> Some (H.Pat.tuple (List.map convert_fun lst))

  let add_transition current_node sub_constructor args =
    let state_node = H.Exp.construct
        (Location.mknoloc (Pfx.cstr @@ Pfx.st current_node))
        (Some (sub_constructor args))
    and node = H.Exp.construct
        (Location.mknoloc (Pfx.cstr @@ Pfx.t current_node)) (Some [%expr tree])
    in
    [%expr
      A.add_transition
        [%e state_node]
        [%e node]
        parent_state
        automaton
    ]

  (** [mk_body current_node sub_constructor [t0; t1]] is the expression:
      [
      let (automaton, state_0) = A.add_state automaton in
      let (automaton, state_1) = A.add_state automaton in
      A.add_transition
        St.(Current_node [%e sub_constructor [t0; t1]])
        (T.Current_node tree)
        parent_state
        automaton
      |> t0 state_0 arg_0
      |> t1 state_1 arg_1
      |> (fun x -> x)
      ]
  *)
  let mk_body
      (current_node: string)
      (sub_constructor : 'a list -> Parsetree.expression)
      (args : string list)
    =
    let final = [%expr fun x -> x]
    and pipe e1 e2 = [%expr [%e e1] |> [%e e2]]
    and generate_sub_automaton (arg_number, typ_name) =
      H.Exp.apply
        (H.Exp.ident (L.mknoloc (LI.Lident typ_name)))
        [
          "", H.Exp.ident (L.mknoloc (LI.Lident (C.nth_state arg_number)));
          "", H.Exp.ident (L.mknoloc (LI.Lident (C.nth_arg   arg_number)));
        ]
    and add_state cont (arg_number, _typ_name) =
      let state = H.Pat.var (L.mknoloc @@ C.nth_state arg_number) in
      [%expr let (automaton, [%p state]) = A.add_state automaton in [%e cont]]
    and args = List.mapi Misc.pair args
    in
    List.fold_left add_state
      (List.fold_left
         (fun accu arg -> pipe (generate_sub_automaton arg) accu)
         final args
       |> pipe @@ add_transition current_node sub_constructor args
      )
      args

  let case_of_constructor type_name constructor =
    let pat =
      let open Types in
      H.Pat.construct
        (L.mknoloc @@ Pfx.t constructor.cd_id.Ident.name)
        (gen_pattern_constr_args constructor.cd_args)
    and result =
      List.map (fun typ ->
          C.id_of_typ_expr typ
        )
        constructor.Types.cd_args
      |> List.flip_opt
      |> Option.map (mk_body type_name (fun _ -> [%expr assert false]))
      |> Option.value [%expr failwith "invalid type"]
    in
    H.Exp.case pat result

  let wrap_into_fun body =
      [%expr fun parent_state tree automaton -> [%e body] ]

  let middle_of_variant name constructors =
    let expr =
      wrap_into_fun
        (H.Exp.match_ [%expr tree]
           (List.map (case_of_constructor name) constructors))
    in
    name, expr

  let middle_of_record name fields =
    let open Types in
    let destructured_record =
      let pattern_fields =
        List.mapi (fun idx field ->
            L.mknoloc @@ Pfx.t field.ld_id.Ident.name,
            H.Pat.var (L.mknoloc @@ C.nth_arg idx)
          )
          fields
      in H.Pat.record pattern_fields Asttypes.Closed
    and body =
        List.map (fun field -> C.id_of_typ_expr field.ld_type) fields
        |> List.flip_opt
        |> Option.map (mk_body name (fun _ -> [%expr assert false]))
        |> Option.value [%expr failwith "invalid type"]
    in
    let expr =
      wrap_into_fun
        [%expr let [%p destructured_record] = tree in [%e body]]
    in
    name, expr

  let middle_of_abstract name = name, [%expr assert false]
  let middle_of_alias name _ _ _ = name, [%expr assert false]

  let result_of_middle middle =
    let bindings = List.map (fun (name, value) ->
        H.Vb.mk
          (H.Pat.var (L.mknoloc name))
          value
      )
        middle
    in
    H.Str.value Asttypes.Recursive bindings :: []
end

include Generator.Make (Arg)
