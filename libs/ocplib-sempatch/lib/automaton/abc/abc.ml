open Std_utils

open Parsetree

let raw_stdlib = String.concat "\ntype "
    [
      "type string";
      "bytes";
      "int";
      "char";
      "bool";
      "'a list = Nil | Cons of 'a * 'a list";
      "'a option = None | Some of 'a";
      "location_t";
      "longident_t";
      "asttypes_constant";
      "asttypes_rec_flag = Nonrecursive | Recursive";
      "asttypes_direction_flag = Upto | Downto";
      "asttypes_private_flag = Private | Public";
      "asttypes_mutable_flag = Immutable | Mutable";
      "asttypes_virtual_flag = Virtual | Concrete";
      "asttypes_override_flag = Override | Fresh";
      "asttypes_closed_flag = Closed | Open";
      "asttypes_label = bytes";
      "'a asttypes_loc = { txt : 'a; loc : location_t; }";
      "asttypes_variance = Covariant | Contravariant | Invariant";
    ]

let wrap_into_module module_name structure =
  let module H = Ast_helper in
  H.Str.module_
    (H.Mb.mk
       (Location.mknoloc module_name)
       (H.Mod.structure structure))

let filter_monomorph = List.filter (fun (_, typ) -> typ.Types.type_params = [])

let stdlib =
  let module T = Types in
  let typed_raw =
    Parser.interface Lexer.token (Lexing.from_string raw_stdlib)
    |> Typemod.type_interface Env.initial_safe_string
    |> (fun psig -> psig.Typedtree.sig_type)
  in
  List.bind (function
      | T.Sig_type (name, value, _) ->
        [name.Ident.name, value]
      | _ -> []
    )
    typed_raw

let signature =
  Cmi_format.(
    (read_cmi
       "/nix/store/3l0mdlja0y2zl5vz0h1qhch046wb85y0\
        -ocaml-4.02.3/lib/ocaml/compiler-libs/parsetree.cmi")
    .cmi_sign
  )

let declarations =
  List.bind
    (
      function
      | Types.Sig_type (ident, decl, _) ->
        [Ident.name ident, decl]
      | _ -> []
    )
    signature
and implicit_decls =
  Polymorphic_types_collector.from_signature signature

let all_decls = stdlib @ declarations @ implicit_decls

let mapper = Ast_mapper.{
    default_mapper with
    structure = (fun _ str ->
        let module H = Ast_helper in
        List.bind (fun stri ->
            match stri.pstr_desc with
            | Pstr_extension ((id, _), _)
              when id.Asttypes.txt = "create_state_tree"
              ->
              [
                State_tree_gen.of_type_decl ~env:[] (all_decls);
                Sum_builder.of_type_decl ~env:[] (all_decls)
                  [Location.mknoloc "deriving", Parsetree.PStr [%str ord]];
              ]
            | Pstr_extension ((id, _), _)
              when id.Asttypes.txt = "create_eval_tree"
              ->
              (wrap_into_module
                 "Nodes"
                 [
                   Abstract_tree.of_type_decl ~env:[] (all_decls);
                   Sum_builder.of_type_decl ~env:[] (all_decls)
                     [];
                 ])
              ::
                [%str
           type nonrec string = string
           type nonrec bool = bool
           type nonrec int = int
           type nonrec char = char
           type nonrec 'a option = 'a option = None | Some of 'a
           type nonrec bytes = bytes
           type location_t = Location.t
           type asttypes_constant = Asttypes.constant
           type asttypes_label = Asttypes.label
           type asttypes_rec_flag = Asttypes.rec_flag = Nonrecursive |
                                                        Recursive
           type asttypes_direction_flag = Asttypes.direction_flag = Upto |
                                                                    Downto
           type asttypes_private_flag = Asttypes.private_flag = Private |
                                                                Public
           type asttypes_mutable_flag = Asttypes.mutable_flag = Immutable |
                                                                Mutable
           type asttypes_virtual_flag = Asttypes.virtual_flag = Virtual |
                                                                Concrete
           type asttypes_override_flag = Asttypes.override_flag = Override |
                                                                  Fresh
           type asttypes_closed_flag = Asttypes.closed_flag =  Closed | Open
           type asttypes_variance = Asttypes.variance = Covariant |
                                                        Contravariant |
                                                        Invariant
           type 'a asttypes_loc = 'a Location.loc = { txt : 'a; loc :
                                                        Location.t; }
           type longident_t = Longident.t
]
@
[
                Types_to_parsetree.of_type_decl ~env:[]
                  (filter_monomorph @@ implicit_decls) None;
                Types_to_parsetree.of_type_decl ~env:[]
                  (declarations) (Some "Parsetree");
                Sum_builder.of_type_decl
                  ~env:[]
                  (filter_monomorph @@ all_decls)
                  [];
                (wrap_into_module
                  "Convert"
                  [
                    Tree_convert.of_type_decl ~env:[] (all_decls);
                    Tree_convert.Sum.of_type_decl
                      ~env:[]
                      (filter_monomorph @@ all_decls);
                  ]);
              ]
            | Pstr_extension ((id, _), _)
              when id.Asttypes.txt = "automaton_builder"
              ->
              Builder_gen.of_type_decl ~env:all_decls
                (filter_monomorph @@ all_decls)
            | _ -> [stri]
          )
          str
      )
  }

let () = Ast_mapper.register "state_tree" (fun _ -> mapper)
