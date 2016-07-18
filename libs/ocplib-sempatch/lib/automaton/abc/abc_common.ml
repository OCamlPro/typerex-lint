open Std_utils

module H = Ast_helper

let id_of_path path =
  Path.name path
  |> String.map (fun c -> if c = '.' then '_' else c)

let rec id_of_typ_expr texpr =
  let open Types in
  match texpr.desc with
  | Ttuple exprs ->
    print_texpr_list exprs
  | Tconstr (path, args, _) ->
    Option.map
      (fun args_str ->  args_str ^ "_" ^ (id_of_path path))
      (print_texpr_list args)
  | _ -> None

and print_texpr_list tlist =
  List.map id_of_typ_expr tlist
  |> List.flip_opt
  |> Option.map (String.concat "_")

let fail = Printf.ksprintf failwith

let rec longident_of_path =
  let module L = Longident in
  let module P = Path in
  function
  | P.Pident i -> L.Lident (Ident.name i)
  | P.Pdot (path, name, _) -> L.Ldot (longident_of_path path, name)
  | P.Papply (lhs, rhs) -> L.Lapply (longident_of_path lhs, longident_of_path rhs)


let rec core_type_of_type_expr texpr =
  let open Types in
  match texpr.desc with
  | Ttuple args ->
    H.Typ.tuple (List.map core_type_of_type_expr args)
  | Tconstr (name, args, _) ->
    H.Typ.constr
      (Location.mknoloc @@ longident_of_path name)
      (List.map core_type_of_type_expr args)
  | _ ->
    fail "This type can't be converted to core_type"
