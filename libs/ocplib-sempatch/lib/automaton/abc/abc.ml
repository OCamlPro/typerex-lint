open Std_utils

open Parsetree

let raw_stdlib = String.concat "\ntype "
    [
      "type string";
      "int";
      "char";
      "bool";
      "'a list = Nil | Cons of 'a * 'a list";
    ]

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

let mapper = Ast_mapper.{
    default_mapper with
    structure = (fun _ str ->
        List.bind (fun stri ->
            match stri.pstr_desc with
            | Pstr_extension ((id, _), _)
              when id.Asttypes.txt = "create_state_tree"
              ->
              let signature =
                Cmi_format.(
                  (read_cmi
                     "/nix/store/3l0mdlja0y2zl5vz0h1qhch046wb85y0\
                      -ocaml-4.02.3/lib/ocaml/compiler-libs/parsetree.cmi")
                  .cmi_sign
                )
              in
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
              in
              let all_decls = declarations @ implicit_decls in
              State_tree_gen.of_type_decl ~env:[]
                all_decls
              :: Builder_gen.of_type_decl ~env:(stdlib @ all_decls)
                all_decls
            | _ -> [stri]
          )
          str
      )
  }

let () = Ast_mapper.register "state_tree" (fun _ -> mapper)
