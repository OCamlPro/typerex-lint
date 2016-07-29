open Std_utils

module H = Ast_helper
module L = Location
module C = Abc_common
module Pfx = C.Pfx

let (!!) = C.(!!)
let unit_expr = [%expr ()]

let wrap_into_fun expr typ_name =
  let _typ = H.Typ.constr (L.mknoloc @@ Longident.Lident typ_name) [] in
  [%expr fun (elt) -> [%e expr]]

module Arg : Generator.ARG with type result = Parsetree.structure_item =
struct
  type result = Parsetree.structure_item
  type middle = Parsetree.value_binding

  let name = "Tree converter"

  let middle_of_record type_name fields =
    let fields =
      List.map
        (
          fun field ->
            Location.mknoloc @@ Pfx.n !!(field.Types.ld_id.Ident.name),
            unit_expr
        )
        fields
    in
    H.Vb.mk
      (H.Pat.var (Location.mknoloc type_name))
      (wrap_into_fun
         [%expr ignore elt; [%e H.Exp.record fields None]]
         type_name)

  let middle_of_variant name constructors =
    let constructors =
      List.map
        (
          fun constructor ->
            {
              Parsetree.pc_lhs =
                H.Pat.construct
                  (Location.mknoloc
                   @@ Pfx.cstr
                   @@ Longident.Lident
                     constructor.Types.cd_id.Ident.name)
                  (if constructor.Types.cd_args <> [] then
                     Some (H.Pat.any ())
                   else None);
              pc_guard = None;
              pc_rhs = H.Exp.construct
                  (Location.mknoloc
                   @@ Pfx.cstr
                   @@ Pfx.n
                     !!(constructor.Types.cd_id.Ident.name))
                  None;
            }
        )
        constructors
    in
    H.Vb.mk
      (H.Pat.var (Location.mknoloc name))
      (wrap_into_fun
         (H.Exp.match_
            [%expr elt]
            constructors)
         name)

  let middle_of_abstract name =
    H.Vb.mk
      (H.Pat.var (Location.mknoloc name))
      (wrap_into_fun unit_expr name)

  let middle_of_alias name _rec _env value =
    match value.Types.desc with
    | Types.Ttuple args ->
      H.Vb.mk
        (H.Pat.var (Location.mknoloc name))
        (wrap_into_fun (H.Exp.tuple (List.map (fun _ -> unit_expr) args)) name)
    | _ ->
      middle_of_abstract name

  let result_of_middle typedefs =
    H.Str.value Asttypes.Nonrecursive typedefs
end

module Arg_sum : Generator.ARG with type result = Parsetree.structure_item =
struct
  type result = Parsetree.structure_item
  type middle = Parsetree.case

  let name = "Convert sum"

  let middle_of_anything name =
    H.Exp.case
      (H.Pat.construct
         (Location.mknoloc @@ Pfx.cstr @@ Longident.Lident name)
         (Some [%pat? node]))
      (H.Exp.construct
         (Location.mknoloc @@ Pfx.cstr @@ Pfx.n name)
         (Some (H.Exp.apply
                  (H.Exp.ident (Location.mknoloc @@ Longident.Lident name))
                  ["", [%expr node]])))

  let middle_of_abstract = middle_of_anything
  let middle_of_record = Fun.flip @@ Fun.const middle_of_anything
  let middle_of_variant = Fun.flip @@ Fun.const middle_of_anything
  let middle_of_alias name _ _ _ = middle_of_anything name

  let result_of_middle middles =
    H.Str.value
      Asttypes.Nonrecursive
      [
        (H.Vb.mk
           [%pat? convert]
           (H.Exp.function_
              middles))
      ]
end

include Generator.Make (Arg)
module Sum = Generator.Make(Arg_sum)
