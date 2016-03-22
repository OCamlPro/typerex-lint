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

type recursion =
  | Recurse_apply
  | Recurse_reject
  | No_recursion

let rec limit_range condition ?(recursion=Recurse_apply) patch = let open Ast_mapper in
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
  in
  {
    patch with (* TODO: complete all fields and remove this line *)
    structure = choose_apply
        condition.test_structure
        patch.structure
        default_mapper.structure;
    value_binding = choose_apply
        condition.test_value_binding
        patch.value_binding
        default_mapper.value_binding;
    pat = choose_apply
        condition.test_pat
        patch.pat
        default_mapper.pat;
    expr = choose_apply
        condition.test_expr
        patch.expr
        default_mapper.expr;
  }
