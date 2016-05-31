module LI = Longident
module L =Location
open Parsetree
open Asttypes
open Std_utils

let print_longident lid = String.concat "__" (LI.flatten lid)

let tuple_constr loc args =
  let constr =
    LI.Lident ("tuple_" ^ (string_of_int (List.length args)))
  in
  Ptyp_constr (L.mkloc constr loc, args)

let rec upprint typ =
  let open Option.Infix in
  match typ.ptyp_desc with
  | Ptyp_constr (id, args) ->
    (Some (print_longident id.txt)) :: List.map upprint args
    |> List.flip_opt
    >|= (String.concat "_")
  | Ptyp_tuple args ->
    upprint {typ with ptyp_desc = tuple_constr typ.ptyp_loc args; }
  | _ -> None

let type_of_string str =
  Parser.parse_core_type Lexer.token (Lexing.from_string str)

let id = String.uncapitalize
let cstr str =
  match String.capitalize str with
  | "Cons" -> "::"
  | "Nil" -> "[]"
  | s -> s

let instantiate_type_decl variables_def typ =
  let mapper = let open Ast_mapper in {
      default_mapper with
      typ = (fun self typ ->
          let mapped =
            match typ.ptyp_desc with
            | Ptyp_var v ->
              begin
                try
                  List.assoc v variables_def
                with
                  Not_found -> typ
              end
            | _ -> typ
          in default_mapper.typ self mapped
        );
    }
    in mapper.Ast_mapper.type_declaration mapper typ

let eprint prefix arg =
  let () = Printf.eprintf "%s : " prefix in
  Printf.eprintf arg
let warn msg = eprint "Warning" msg
let debug msg = eprint "Debug" msg

let stdlib =
  let module H = Ast_helper in
  [
    H.Type.mk
      ~params:[[%type: 'a], Invariant]
      ~kind:(Ptype_variant [
          H.Type.constructor
            (L.mknoloc "Nil");
          H.Type.constructor
            ~args:[[%type: 'a]; [%type: 'a list]]
            (L.mknoloc "Cons");
        ])
      (L.mknoloc "list");
    H.Type.mk
      ~params:[[%type: 'a], Invariant]
      ~kind:(Ptype_variant [
          H.Type.constructor
            (L.mknoloc "None");
          H.Type.constructor
            ~args:[[%type: 'a]]
            (L.mknoloc "Some");
        ])
      (L.mknoloc "option");
    H.Type.mk (L.mknoloc "unit");
    H.Type.mk (L.mknoloc "bool");
    H.Type.mk (L.mknoloc "int");
    H.Type.mk (L.mknoloc "char");
    H.Type.mk (L.mknoloc "string");
    H.Type.mk (L.mknoloc "int32");
    H.Type.mk (L.mknoloc "int64");
    H.Type.mk (L.mknoloc "nativeint");
    H.Type.mk (L.mknoloc "location__t");
  ]

(* [filter_decls type_decls] returns the list of monomorphic type
declarations in type_decls *)
let filter_decls = List.filter (fun decl -> decl.ptype_params = [])

let concrete_stdlib = filter_decls stdlib
