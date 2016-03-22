open Parsetree

type t = {
  test_attribute: t -> attribute -> bool * t;
  test_attributes: t -> attributes -> bool * t;
  test_case: t -> case -> bool * t;
  test_cases: t -> case list -> bool * t;
  test_class_declaration: t -> class_declaration -> bool * t;
  test_class_description: t -> class_description -> bool * t;
  test_class_expr: t -> class_expr -> bool * t;
  test_class_field: t -> class_field -> bool * t;
  test_class_signature: t -> class_signature -> bool * t;
  test_class_structure: t -> class_structure -> bool * t;
  test_class_type: t -> class_type -> bool * t;
  test_class_type_declaration: t -> class_type_declaration -> bool * t;
  test_class_type_field: t -> class_type_field -> bool * t;
  test_constructor_declaration: t -> constructor_declaration -> bool * t;
  test_expr: t -> expression -> bool * t;
  test_extension: t -> extension -> bool * t;
  test_extension_constructor: t -> extension_constructor -> bool * t;
  test_include_declaration: t -> include_declaration -> bool * t;
  test_include_description: t -> include_description -> bool * t;
  test_label_declaration: t -> label_declaration -> bool * t;
  test_location: t -> Location.t -> bool * t;
  test_module_binding: t -> module_binding -> bool * t;
  test_module_declaration: t -> module_declaration -> bool * t;
  test_module_expr: t -> module_expr -> bool * t;
  test_module_type: t -> module_type -> bool * t;
  test_module_type_declaration: t -> module_type_declaration -> bool * t;
  test_open_description: t -> open_description -> bool * t;
  test_pat: t -> pattern -> bool * t;
  test_payload: t -> payload -> bool * t;
  test_signature: t -> signature -> bool * t;
  test_signature_item: t -> signature_item -> bool * t;
  test_structure: t -> structure -> bool * t;
  test_structure_item: t -> structure_item -> bool * t;
  test_typ: t -> core_type -> bool * t;
  test_type_declaration: t -> type_declaration -> bool * t;
  test_type_extension: t -> type_extension -> bool * t;
  test_type_kind: t -> type_kind -> bool * t;
  test_value_binding: t -> value_binding -> bool * t;
  test_value_description: t -> value_description -> bool * t;
  test_with_constraint: t -> with_constraint -> bool * t;
}

let rec all = {
  test_attribute = (fun f _ -> true, f);
  test_attributes = (fun f _ -> true, f);
  test_case = (fun f _ -> true, f);
  test_cases = (fun f _ -> true, f);
  test_class_declaration = (fun f _ -> true, f);
  test_class_description = (fun f _ -> true, f);
  test_class_expr = (fun f _ -> true, f);
  test_class_field = (fun f _ -> true, f);
  test_class_signature = (fun f _ -> true, f);
  test_class_structure = (fun f _ -> true, f);
  test_class_type = (fun f _ -> true, f);
  test_class_type_declaration = (fun f _ -> true, f);
  test_class_type_field = (fun f _ -> true, f);
  test_constructor_declaration = (fun f _ -> true, f);
  test_expr = (fun f _ -> true, f);
  test_extension = (fun f _ -> true, f);
  test_extension_constructor = (fun f _ -> true, f);
  test_include_declaration = (fun f _ -> true, f);
  test_include_description = (fun f _ -> true, f);
  test_label_declaration = (fun f _ -> true, f);
  test_location = (fun f _ -> true, f);
  test_module_binding = (fun f _ -> true, f);
  test_module_declaration = (fun f _ -> true, f);
  test_module_expr = (fun f _ -> true, f);
  test_module_type = (fun f _ -> true, f);
  test_module_type_declaration = (fun f _ -> true, f);
  test_open_description = (fun f _ -> true, f);
  test_pat = (fun f _ -> true, f);
  test_payload = (fun f _ -> true, f);
  test_signature = (fun f _ -> true, f);
  test_signature_item = (fun f _ -> true, f);
  test_structure = (fun f _ -> true, f);
  test_structure_item = (fun f _ -> true, f);
  test_typ = (fun f _ -> true, f);
  test_type_declaration = (fun f _ -> true, f);
  test_type_extension = (fun f _ -> true, f);
  test_type_kind = (fun f _ -> true, f);
  test_value_binding = (fun f _ -> true, f);
  test_value_description = (fun f _ -> true, f);
  test_with_constraint = (fun f _ -> true, f);
}

let rec nothing = {
  test_attribute = (fun f _ -> false, f);
  test_attributes = (fun f _ -> false, f);
  test_case = (fun f _ -> false, f);
  test_cases = (fun f _ -> false, f);
  test_class_declaration = (fun f _ -> false, f);
  test_class_description = (fun f _ -> false, f);
  test_class_expr = (fun f _ -> false, f);
  test_class_field = (fun f _ -> false, f);
  test_class_signature = (fun f _ -> false, f);
  test_class_structure = (fun f _ -> false, f);
  test_class_type = (fun f _ -> false, f);
  test_class_type_declaration = (fun f _ -> false, f);
  test_class_type_field = (fun f _ -> false, f);
  test_constructor_declaration = (fun f _ -> false, f);
  test_expr = (fun f _ -> false, f);
  test_extension = (fun f _ -> false, f);
  test_extension_constructor = (fun f _ -> false, f);
  test_include_declaration = (fun f _ -> false, f);
  test_include_description = (fun f _ -> false, f);
  test_label_declaration = (fun f _ -> false, f);
  test_location = (fun f _ -> false, f);
  test_module_binding = (fun f _ -> false, f);
  test_module_declaration = (fun f _ -> false, f);
  test_module_expr = (fun f _ -> false, f);
  test_module_type = (fun f _ -> false, f);
  test_module_type_declaration = (fun f _ -> false, f);
  test_open_description = (fun f _ -> false, f);
  test_pat = (fun f _ -> false, f);
  test_payload = (fun f _ -> false, f);
  test_signature = (fun f _ -> false, f);
  test_signature_item = (fun f _ -> false, f);
  test_structure = (fun f _ -> false, f);
  test_structure_item = (fun f _ -> false, f);
  test_typ = (fun f _ -> false, f);
  test_type_declaration = (fun f _ -> false, f);
  test_type_extension = (fun f _ -> false, f);
  test_type_kind = (fun f _ -> false, f);
  test_value_binding = (fun f _ -> false, f);
  test_value_description = (fun f _ -> false, f);
  test_with_constraint = (fun f _ -> false, f);
}

let rec limit_range condition patch = let open Ast_mapper in
  let choose_apply
      cond
      trans
      default_trans
      parent_mapper
      node
    =
    match cond condition node with
    | (true, sub) -> trans (limit_range sub patch) node
    | (false, sub) -> default_trans (limit_range sub patch) node
  in {
    attribute = choose_apply
        condition.test_attribute
        patch.attribute
        default_mapper.attribute;
    attributes = choose_apply
        condition.test_attributes
        patch.attributes
        default_mapper.attributes;
    case = choose_apply
        condition.test_case
        patch.case
        default_mapper.case;
    cases = choose_apply
        condition.test_cases
        patch.cases
        default_mapper.cases;
    class_declaration = choose_apply
        condition.test_class_declaration
        patch.class_declaration
        default_mapper.class_declaration;
    class_description = choose_apply
        condition.test_class_description
        patch.class_description
        default_mapper.class_description;
    class_expr = choose_apply
        condition.test_class_expr
        patch.class_expr
        default_mapper.class_expr;
    class_field = choose_apply
        condition.test_class_field
        patch.class_field
        default_mapper.class_field;
    class_signature = choose_apply
        condition.test_class_signature
        patch.class_signature
        default_mapper.class_signature;
    class_structure = choose_apply
        condition.test_class_structure
        patch.class_structure
        default_mapper.class_structure;
    class_type = choose_apply
        condition.test_class_type
        patch.class_type
        default_mapper.class_type;
    class_type_declaration = choose_apply
        condition.test_class_type_declaration
        patch.class_type_declaration
        default_mapper.class_type_declaration;
    class_type_field = choose_apply
        condition.test_class_type_field
        patch.class_type_field
        default_mapper.class_type_field;
    constructor_declaration = choose_apply
        condition.test_constructor_declaration
        patch.constructor_declaration
        default_mapper.constructor_declaration;
    expr = choose_apply
        condition.test_expr
        patch.expr
        default_mapper.expr;
    extension = choose_apply
        condition.test_extension
        patch.extension
        default_mapper.extension;
    extension_constructor = choose_apply
        condition.test_extension_constructor
        patch.extension_constructor
        default_mapper.extension_constructor;
    include_declaration = choose_apply
        condition.test_include_declaration
        patch.include_declaration
        default_mapper.include_declaration;
    include_description = choose_apply
        condition.test_include_description
        patch.include_description
        default_mapper.include_description;
    label_declaration = choose_apply
        condition.test_label_declaration
        patch.label_declaration
        default_mapper.label_declaration;
    location = choose_apply
        condition.test_location
        patch.location
        default_mapper.location;
    module_binding = choose_apply
        condition.test_module_binding
        patch.module_binding
        default_mapper.module_binding;
    module_declaration = choose_apply
        condition.test_module_declaration
        patch.module_declaration
        default_mapper.module_declaration;
    module_expr = choose_apply
        condition.test_module_expr
        patch.module_expr
        default_mapper.module_expr;
    module_type = choose_apply
        condition.test_module_type
        patch.module_type
        default_mapper.module_type;
    module_type_declaration = choose_apply
        condition.test_module_type_declaration
        patch.module_type_declaration
        default_mapper.module_type_declaration;
    open_description = choose_apply
        condition.test_open_description
        patch.open_description
        default_mapper.open_description;
    pat = choose_apply
        condition.test_pat
        patch.pat
        default_mapper.pat;
    payload = choose_apply
        condition.test_payload
        patch.payload
        default_mapper.payload;
    signature = choose_apply
        condition.test_signature
        patch.signature
        default_mapper.signature;
    signature_item = choose_apply
        condition.test_signature_item
        patch.signature_item
        default_mapper.signature_item;
    structure = choose_apply
        condition.test_structure
        patch.structure
        default_mapper.structure;
    structure_item = choose_apply
        condition.test_structure_item
        patch.structure_item
        default_mapper.structure_item;
    typ = choose_apply
        condition.test_typ
        patch.typ
        default_mapper.typ;
    type_declaration = choose_apply
        condition.test_type_declaration
        patch.type_declaration
        default_mapper.type_declaration;
    type_extension = choose_apply
        condition.test_type_extension
        patch.type_extension
        default_mapper.type_extension;
    type_kind = choose_apply
        condition.test_type_kind
        patch.type_kind
        default_mapper.type_kind;
    value_binding = choose_apply
        condition.test_value_binding
        patch.value_binding
        default_mapper.value_binding;
    value_description = choose_apply
        condition.test_value_description
        patch.value_description
        default_mapper.value_description;
    with_constraint = choose_apply
        condition.test_with_constraint
        patch.with_constraint
        default_mapper.with_constraint;
  }
