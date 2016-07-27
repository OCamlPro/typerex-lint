open Std_utils

open Parsetree

let raw_stdlib = String.concat "\ntype "
    [
      "type string";
      "int";
      "char";
      "bool";
      "'a list = Nil | Cons of 'a * 'a list";
      "'a option = None | Some of 'a";
      "'a asttypes__loc";
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

let deriving = Abc_common.deriving

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

let all_decls = declarations @ implicit_decls

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
                State_tree_gen.of_type_decl ~env:[] (all_decls @ stdlib);
                Sum_builder.of_type_decl ~env:[] (all_decls @ stdlib)
                  [Location.mknoloc "deriving", Parsetree.PStr [%str ord]];
              ]
            | Pstr_extension ((id, _), _)
              when id.Asttypes.txt = "create_eval_tree"
              ->
              (wrap_into_module
                 "Nodes"
                 [
                   Abstract_tree.of_type_decl ~env:[] all_decls;
                   Sum_builder.of_type_decl ~env:[] all_decls deriving;
                 ])
              ::
              [
                H.Str.include_
                  (H.Incl.mk
                     (H.Mod.ident
                        (Location.mknoloc @@ Longident.Lident "Parsetree")));
                Types_to_parsetree.of_type_decl ~env:[] implicit_decls;
                Sum_builder.of_type_decl
                  ~env:[]
                  (filter_monomorph all_decls)
                  [];
                (wrap_into_module
                  "Convert"
                  [
                    Tree_convert.of_type_decl ~env:[] all_decls;
                    Tree_convert.Sum.of_type_decl
                      ~env:[]
                      (filter_monomorph all_decls);
                  ]);
              ]
            | Pstr_extension ((id, _), _)
              when id.Asttypes.txt = "automaton_builder"
              ->
              Builder_gen.of_type_decl ~env:(stdlib @ all_decls)
                all_decls
            | _ -> [stri]
          )
          str
      )
  }

let () = Ast_mapper.register "state_tree" (fun _ -> mapper)
