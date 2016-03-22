open Parsetree

type node_chooser = {
  test_attribute: attribute -> bool;
  test_attributes: attributes -> bool;
  test_case: case -> bool;
  test_cases: case list -> bool;
  test_class_declaration: class_declaration -> bool;
  test_class_description: class_description -> bool;
  test_class_expr: class_expr -> bool;
  test_class_field: class_field -> bool;
  test_class_signature: class_signature -> bool;
  test_class_structure: class_structure -> bool;
  test_class_type: class_type -> bool;
  test_class_type_declaration: class_type_declaration -> bool;
  test_class_type_field: class_type_field -> bool;
  test_constructor_declaration: constructor_declaration -> bool;
  test_expr: expression -> bool;
  test_extension: extension -> bool;
  test_extension_constructor: extension_constructor -> bool;
  test_include_declaration: include_declaration -> bool;
  test_include_description: include_description -> bool;
  test_label_declaration: label_declaration -> bool;
  test_location: Location.t -> bool;
  test_module_binding: module_binding -> bool;
  test_module_declaration: module_declaration -> bool;
  test_module_expr: module_expr -> bool;
  test_module_type: module_type -> bool;
  test_module_type_declaration: module_type_declaration -> bool;
  test_open_description: open_description -> bool;
  test_pat: pattern -> bool;
  test_payload: payload -> bool;
  test_signature: signature -> bool;
  test_signature_item: signature_item -> bool;
  test_structure: structure -> bool;
  test_structure_item: structure_item -> bool;
  test_typ: core_type -> bool;
  test_type_declaration: type_declaration -> bool;
  test_type_extension: type_extension -> bool;
  test_type_kind: type_kind -> bool;
  test_value_binding: value_binding -> bool;
  test_value_description: value_description -> bool;
  test_with_constraint: with_constraint -> bool;
}

type recursion =
  | Recurse_apply
  | Recurse_reject
  | No_recursion

let always_allow = {
  test_attribute = (fun _ -> true);
  test_attributes = (fun _ -> true);
  test_case = (fun _ -> true);
  test_cases = (fun _ -> true);
  test_class_declaration = (fun _ -> true);
  test_class_description = (fun _ -> true);
  test_class_expr = (fun _ -> true);
  test_class_field = (fun _ -> true);
  test_class_signature = (fun _ -> true);
  test_class_structure = (fun _ -> true);
  test_class_type = (fun _ -> true);
  test_class_type_declaration = (fun _ -> true);
  test_class_type_field = (fun _ -> true);
  test_constructor_declaration = (fun _ -> true);
  test_expr = (fun _ -> true);
  test_extension = (fun _ -> true);
  test_extension_constructor = (fun _ -> true);
  test_include_declaration = (fun _ -> true);
  test_include_description = (fun _ -> true);
  test_label_declaration = (fun _ -> true);
  test_location = (fun _ -> true);
  test_module_binding = (fun _ -> true);
  test_module_declaration = (fun _ -> true);
  test_module_expr = (fun _ -> true);
  test_module_type = (fun _ -> true);
  test_module_type_declaration = (fun _ -> true);
  test_open_description = (fun _ -> true);
  test_pat = (fun _ -> true);
  test_payload = (fun _ -> true);
  test_signature = (fun _ -> true);
  test_signature_item = (fun _ -> true);
  test_structure = (fun _ -> true);
  test_structure_item = (fun _ -> true);
  test_typ = (fun _ -> true);
  test_type_declaration = (fun _ -> true);
  test_type_extension = (fun _ -> true);
  test_type_kind = (fun _ -> true);
  test_value_binding = (fun _ -> true);
  test_value_description = (fun _ -> true);
  test_with_constraint = (fun _ -> true);
}

let always_reject ={
  test_attribute = (fun _ -> false);
  test_attributes = (fun _ -> false);
  test_case = (fun _ -> false);
  test_cases = (fun _ -> false);
  test_class_declaration = (fun _ -> false);
  test_class_description = (fun _ -> false);
  test_class_expr = (fun _ -> false);
  test_class_field = (fun _ -> false);
  test_class_signature = (fun _ -> false);
  test_class_structure = (fun _ -> false);
  test_class_type = (fun _ -> false);
  test_class_type_declaration = (fun _ -> false);
  test_class_type_field = (fun _ -> false);
  test_constructor_declaration = (fun _ -> false);
  test_expr = (fun _ -> false);
  test_extension = (fun _ -> false);
  test_extension_constructor = (fun _ -> false);
  test_include_declaration = (fun _ -> false);
  test_include_description = (fun _ -> false);
  test_label_declaration = (fun _ -> false);
  test_location = (fun _ -> false);
  test_module_binding = (fun _ -> false);
  test_module_declaration = (fun _ -> false);
  test_module_expr = (fun _ -> false);
  test_module_type = (fun _ -> false);
  test_module_type_declaration = (fun _ -> false);
  test_open_description = (fun _ -> false);
  test_pat = (fun _ -> false);
  test_payload = (fun _ -> false);
  test_signature = (fun _ -> false);
  test_signature_item = (fun _ -> false);
  test_structure = (fun _ -> false);
  test_structure_item = (fun _ -> false);
  test_typ = (fun _ -> false);
  test_type_declaration = (fun _ -> false);
  test_type_extension = (fun _ -> false);
  test_type_kind = (fun _ -> false);
  test_value_binding = (fun _ -> false);
  test_value_description = (fun _ -> false);
  test_with_constraint = (fun _ -> false);
}

let rec limit_range condition ?(recursion=No_recursion) patch = let open Ast_mapper in
  let choose_apply
      cond
      trans
      default_trans
      parent_mapper
      node
    =
    if cond node then
      let recursive_call = if recursion=Recurse_apply then patch else parent_mapper in
      trans recursive_call node
    else
    if recursion=Recurse_reject then
      node
    else
      default_trans parent_mapper node
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
