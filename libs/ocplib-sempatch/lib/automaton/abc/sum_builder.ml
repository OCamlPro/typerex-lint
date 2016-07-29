open Std_utils

module H = Ast_helper
module L = Location
module C = Abc_common

module Arg : Generator.ARG
  with type result = Parsetree.attributes -> Parsetree.structure_item =
struct
  type result = Parsetree.attributes -> Parsetree.structure_item
  type middle = Parsetree.constructor_declaration

  let name = "Sum of types"

  let middle_of_anything type_name =
    H.Type.constructor
      ~args:[H.Typ.constr (L.mknoloc @@ Longident.Lident type_name) []]
      (L.mknoloc @@ String.capitalize type_name)

  let middle_of_abstract = middle_of_anything
  let middle_of_record = Fun.flip @@ Fun.const middle_of_anything
  let middle_of_variant = Fun.flip @@ Fun.const middle_of_anything
  let middle_of_alias name _ _ _ = middle_of_anything name

  let result_of_middle middles attrs =
    H.Str.type_
      [
        H.Type.mk
          ~attrs
          ~kind:(Parsetree.Ptype_variant middles)
          (L.mknoloc "t")
      ]
end

include Generator.Make (Arg)
