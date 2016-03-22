type t = {
  test_attribute : t -> Parsetree.attribute -> bool * t;
  test_attributes : t -> Parsetree.attributes -> bool * t;
  test_case : t -> Parsetree.case -> bool * t;
  test_cases : t -> Parsetree.case list -> bool * t;
  test_class_declaration : t -> Parsetree.class_declaration -> bool * t;
  test_class_description : t -> Parsetree.class_description -> bool * t;
  test_class_expr : t -> Parsetree.class_expr -> bool * t;
  test_class_field : t -> Parsetree.class_field -> bool * t;
  test_class_signature : t -> Parsetree.class_signature -> bool * t;
  test_class_structure : t -> Parsetree.class_structure -> bool * t;
  test_class_type : t -> Parsetree.class_type -> bool * t;
  test_class_type_declaration :
    t -> Parsetree.class_type_declaration -> bool * t;
  test_class_type_field : t -> Parsetree.class_type_field -> bool * t;
  test_constructor_declaration :
    t -> Parsetree.constructor_declaration -> bool * t;
  test_expr : t -> Parsetree.expression -> bool * t;
  test_extension : t -> Parsetree.extension -> bool * t;
  test_extension_constructor :
    t -> Parsetree.extension_constructor -> bool * t;
  test_include_declaration : t -> Parsetree.include_declaration -> bool * t;
  test_include_description : t -> Parsetree.include_description -> bool * t;
  test_label_declaration : t -> Parsetree.label_declaration -> bool * t;
  test_location : t -> Location.t -> bool * t;
  test_module_binding : t -> Parsetree.module_binding -> bool * t;
  test_module_declaration : t -> Parsetree.module_declaration -> bool * t;
  test_module_expr : t -> Parsetree.module_expr -> bool * t;
  test_module_type : t -> Parsetree.module_type -> bool * t;
  test_module_type_declaration :
    t -> Parsetree.module_type_declaration -> bool * t;
  test_open_description : t -> Parsetree.open_description -> bool * t;
  test_pat : t -> Parsetree.pattern -> bool * t;
  test_payload : t -> Parsetree.payload -> bool * t;
  test_signature : t -> Parsetree.signature -> bool * t;
  test_signature_item : t -> Parsetree.signature_item -> bool * t;
  test_structure : t -> Parsetree.structure -> bool * t;
  test_structure_item : t -> Parsetree.structure_item -> bool * t;
  test_typ : t -> Parsetree.core_type -> bool * t;
  test_type_declaration : t -> Parsetree.type_declaration -> bool * t;
  test_type_extension : t -> Parsetree.type_extension -> bool * t;
  test_type_kind : t -> Parsetree.type_kind -> bool * t;
  test_value_binding : t -> Parsetree.value_binding -> bool * t;
  test_value_description : t -> Parsetree.value_description -> bool * t;
  test_with_constraint : t -> Parsetree.with_constraint -> bool * t;
}

val all : t
val nothing : t
val limit_range : t -> Ast_mapper.mapper -> Ast_mapper.mapper
