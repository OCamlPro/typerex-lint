open Parsetree
module M = Ast_mapper
module C = Common
module H = Ast_helper

(** Puts the list of all instanciated polymorphic types (and tuples)
    met in the [met_types] list ref, and returns the same tree with types
    replaced by their aliases *)
let typ_collector met_types =
  let add_if_ok typ =
    match C.upprint typ with
    | Some name ->
      met_types := (C.id name, typ) :: !met_types;
      typ
    | None -> typ
  in
  {
    M.default_mapper with
    M.typ = (fun self typ ->
        let typ = M.(default_mapper.typ self typ) in
        match typ.ptyp_desc with
        | Ptyp_tuple _
        | Ptyp_constr (_, _::_) ->
          add_if_ok typ
        | _ ->
          typ
      );
  }

(** [declare_core_typ name typ] creates a type declaration of the form [let
    name = typ] *)
let declare_core_typ loc name typ =
  H.Type.mk
    ~manifest:typ
    (Location.mkloc name loc)

(** [collect type_declarations] returns the list of type declarations
    [let id = typ] where typ is an instantiated constructor appearing in
    type_declarations (like [int list] of [string option], and id a unique
    identifier for the type *)
let collect = function
  | [] -> []
  | hd::_ as type_decls ->
    let loc = hd.ptype_loc in
    let types_list = ref [] in
    let collector = typ_collector types_list in
    let _ = List.map
        (collector.M.type_declaration collector)
        type_decls
    in
    List.sort_uniq (fun (id1, _) (id2, _) ->
        String.compare id1 id2
      )
      !types_list
    |> List.map (fun (id, def) -> declare_core_typ loc id def)
