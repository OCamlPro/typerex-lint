open Std_utils

module H = Ast_helper
module L = Location
module LI = Longident
module C = Abc_common
module Pfx = Abc_common.Pfx

let (!!) s = s ^ "_"

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

  let gen_expr_constr_states args =
    let convert_fun = fun x -> H.Exp.ident (L.mknoloc (LI.Lident x)) in
    match C.gen_states args with
    | [] -> None
    | [elt] -> Some (convert_fun elt)
    | lst -> Some (H.Exp.tuple (List.map convert_fun lst))

  let add_transition current_node sub_constructor =
    let state_node = H.Exp.construct
        (Location.mknoloc (Pfx.cstr @@ Pfx.st current_node))
        sub_constructor
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
      (sub_constructor : Parsetree.expression option)
      (args : string list)
    =
    let final = [%expr fun x -> x]
    and pipe e2 e1 = [%expr [%e e1] |> [%e e2]]
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
    and args = List.mapi Misc.pair (List.rev args)
    in
    List.fold_left add_state
      (List.fold_left
         (fun accu arg -> pipe (generate_sub_automaton arg) accu)
         (add_transition current_node sub_constructor)
         (List.rev args)
      )
      args
  let mk_state_constr constr =
    let constructor = L.mknoloc @@ Pfx.st !!(constr.Types.cd_id.Ident.name)
    and args = gen_expr_constr_states constr.Types.cd_args
    in
    H.Exp.construct constructor args

  let case_of_constructor type_name constructor =
    let pat =
      let open Types in
      H.Pat.construct
        (L.mknoloc @@ Pfx.cstr @@ Pfx.t constructor.cd_id.Ident.name)
        (gen_pattern_constr_args constructor.cd_args)
    and result =
      List.map (fun typ ->
          C.id_of_typ_expr typ
        )
        constructor.Types.cd_args
      |> List.flip_opt
      |> Option.map (mk_body type_name (Some (mk_state_constr constructor)))
      |> Option.value [%expr failwith "invalid type"]
    in
    H.Exp.case pat result

  let wrap_into_fun body tree_type =
    [%expr fun parent_state (tree : [%t tree_type]) (automaton : A.t) ->
      ([%e body] : A.t)
    ]

  let middle_of_variant name constructors =
    let expr =
      wrap_into_fun
        (H.Exp.match_ [%expr tree]
           (List.map (case_of_constructor name) constructors))
        (H.Typ.constr (L.mknoloc @@ Pfx.t name) [])
    in
    name, expr

  let mk_state_record fields =
    let new_fields =
      List.mapi (fun i field ->
          let name = L.mknoloc @@ Pfx.st !!(field.Types.ld_id.Ident.name)
          and value = H.Exp.ident (L.mknoloc @@ LI.Lident (C.nth_state i))
          in
          name, value
        )
        fields
    in
    H.Exp.record new_fields None

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
      |> Option.map (mk_body name (Some (mk_state_record fields)))
      |> Option.value [%expr failwith "invalid type"]
    in
    let expr =
      wrap_into_fun
        [%expr let [%p destructured_record] = tree in [%e body]]
        (H.Typ.constr (L.mknoloc @@ Pfx.t name) [])
    in
    name, expr

  let middle_of_abstract name =
    let cont =
        (add_transition name (Some [%expr state_0]))
    in
    let expr =
      wrap_into_fun
        [%expr let (automaton, state_0) = A.add_state automaton in [%e cont]]
        (H.Typ.constr (L.mknoloc @@ Pfx.t name) [])
    in
    name, expr

  let middle_of_alias name super env alias =
    let module T = Types in
    match alias.T.desc with
    | T.Ttuple sub_types ->
      let destructured_tuple =
        (Option.value (H.Pat.tuple []) (gen_pattern_constr_args sub_types))
      and body =
        List.map C.id_of_typ_expr sub_types
        |> List.flip_opt
        |> Option.map (mk_body name (gen_expr_constr_states sub_types))
        |> Option.value [%expr failwith "invalid type"]
      in
      let expr =
        wrap_into_fun
          [%expr let [%p destructured_tuple] = tree in [%e body]]
          (H.Typ.constr (L.mknoloc @@ Pfx.t name) [])
      in
      name, expr
    | T.Tconstr (constr_path, [], _) -> (* Just a type alias *)
      let alias_name = C.id_of_path constr_path in
      name,
      [%expr fun x -> [%e H.Exp.ident (L.mknoloc @@ LI.Lident alias_name)] x]
    |T.Tconstr (constr_path, _, _) -> (* A real contructor *)
      let alias_name = C.id_of_path constr_path in
      begin
        match C.get_type alias_name env with
        | None ->
          Messages.warn
            "Invalid type %s, assuming it is abstract\n"
            name;
          middle_of_abstract name
        | Some id ->
          Messages.debug
            "Recursing into type %s\n"
            name;
          super (name, id)
      end
    | _ -> name, [%expr failwith "Not handled by abc"]


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
