type filter_leaf = {
  test_attribute : filter_leaf -> Parsetree.attribute -> bool * filter_leaf;
  test_attributes : filter_leaf -> Parsetree.attributes -> bool * filter_leaf;
  test_case : filter_leaf -> Parsetree.case -> bool * filter_leaf;
  test_cases : filter_leaf -> Parsetree.case list -> bool * filter_leaf;
  test_class_declaration : filter_leaf -> Parsetree.class_declaration -> bool * filter_leaf;
  test_class_description : filter_leaf -> Parsetree.class_description -> bool * filter_leaf;
  test_class_expr : filter_leaf -> Parsetree.class_expr -> bool * filter_leaf;
  test_class_field : filter_leaf -> Parsetree.class_field -> bool * filter_leaf;
  test_class_signature : filter_leaf -> Parsetree.class_signature -> bool * filter_leaf;
  test_class_structure : filter_leaf -> Parsetree.class_structure -> bool * filter_leaf;
  test_class_type : filter_leaf -> Parsetree.class_type -> bool * filter_leaf;
  test_class_type_declaration :
    filter_leaf -> Parsetree.class_type_declaration -> bool * filter_leaf;
  test_class_type_field : filter_leaf -> Parsetree.class_type_field -> bool * filter_leaf;
  test_constructor_declaration :
    filter_leaf -> Parsetree.constructor_declaration -> bool * filter_leaf;
  test_expr : filter_leaf -> Parsetree.expression -> bool * filter_leaf;
  test_extension : filter_leaf -> Parsetree.extension -> bool * filter_leaf;
  test_extension_constructor :
    filter_leaf -> Parsetree.extension_constructor -> bool * filter_leaf;
  test_include_declaration : filter_leaf -> Parsetree.include_declaration -> bool * filter_leaf;
  test_include_description : filter_leaf -> Parsetree.include_description -> bool * filter_leaf;
  test_label_declaration : filter_leaf -> Parsetree.label_declaration -> bool * filter_leaf;
  test_location : filter_leaf -> Location.t -> bool * filter_leaf;
  test_module_binding : filter_leaf -> Parsetree.module_binding -> bool * filter_leaf;
  test_module_declaration : filter_leaf -> Parsetree.module_declaration -> bool * filter_leaf;
  test_module_expr : filter_leaf -> Parsetree.module_expr -> bool * filter_leaf;
  test_module_type : filter_leaf -> Parsetree.module_type -> bool * filter_leaf;
  test_module_type_declaration :
    filter_leaf -> Parsetree.module_type_declaration -> bool * filter_leaf;
  test_open_description : filter_leaf -> Parsetree.open_description -> bool * filter_leaf;
  test_pat : filter_leaf -> Parsetree.pattern -> bool * filter_leaf;
  test_payload : filter_leaf -> Parsetree.payload -> bool * filter_leaf;
  test_signature : filter_leaf -> Parsetree.signature -> bool * filter_leaf;
  test_signature_item : filter_leaf -> Parsetree.signature_item -> bool * filter_leaf;
  test_structure : filter_leaf -> Parsetree.structure -> bool * filter_leaf;
  test_structure_item : filter_leaf -> Parsetree.structure_item -> bool * filter_leaf;
  test_typ : filter_leaf -> Parsetree.core_type -> bool * filter_leaf;
  test_type_declaration : filter_leaf -> Parsetree.type_declaration -> bool * filter_leaf;
  test_type_extension : filter_leaf -> Parsetree.type_extension -> bool * filter_leaf;
  test_type_kind : filter_leaf -> Parsetree.type_kind -> bool * filter_leaf;
  test_value_binding : filter_leaf -> Parsetree.value_binding -> bool * filter_leaf;
  test_value_description : filter_leaf -> Parsetree.value_description -> bool * filter_leaf;
  test_with_constraint : filter_leaf -> Parsetree.with_constraint -> bool * filter_leaf;
}

type t =
  | Test of filter_leaf
  | And of t * t
  | Or of t * t
  | Not of t

val all : filter_leaf
val nothing : filter_leaf
val (&@): t -> t -> t
val (|@): t -> t -> t
val (~@): t -> t
val not_: t -> t

val limit_range : t -> Ast_mapper.mapper -> Ast_mapper.mapper
