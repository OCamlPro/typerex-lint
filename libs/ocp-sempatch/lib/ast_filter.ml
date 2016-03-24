open Parsetree

type filter_leaf = {
  test_attribute: filter_leaf -> attribute -> bool * filter_leaf;
  test_attributes: filter_leaf -> attributes -> bool * filter_leaf;
  test_case: filter_leaf -> case -> bool * filter_leaf;
  test_cases: filter_leaf -> case list -> bool * filter_leaf;
  test_class_declaration: filter_leaf -> class_declaration -> bool * filter_leaf;
  test_class_description: filter_leaf -> class_description -> bool * filter_leaf;
  test_class_expr: filter_leaf -> class_expr -> bool * filter_leaf;
  test_class_field: filter_leaf -> class_field -> bool * filter_leaf;
  test_class_signature: filter_leaf -> class_signature -> bool * filter_leaf;
  test_class_structure: filter_leaf -> class_structure -> bool * filter_leaf;
  test_class_type: filter_leaf -> class_type -> bool * filter_leaf;
  test_class_type_declaration: filter_leaf -> class_type_declaration -> bool * filter_leaf;
  test_class_type_field: filter_leaf -> class_type_field -> bool * filter_leaf;
  test_constructor_declaration: filter_leaf -> constructor_declaration -> bool * filter_leaf;
  test_expr: filter_leaf -> expression -> bool * filter_leaf;
  test_extension: filter_leaf -> extension -> bool * filter_leaf;
  test_extension_constructor: filter_leaf -> extension_constructor -> bool * filter_leaf;
  test_include_declaration: filter_leaf -> include_declaration -> bool * filter_leaf;
  test_include_description: filter_leaf -> include_description -> bool * filter_leaf;
  test_label_declaration: filter_leaf -> label_declaration -> bool * filter_leaf;
  test_location: filter_leaf -> Location.t -> bool * filter_leaf;
  test_module_binding: filter_leaf -> module_binding -> bool * filter_leaf;
  test_module_declaration: filter_leaf -> module_declaration -> bool * filter_leaf;
  test_module_expr: filter_leaf -> module_expr -> bool * filter_leaf;
  test_module_type: filter_leaf -> module_type -> bool * filter_leaf;
  test_module_type_declaration: filter_leaf -> module_type_declaration -> bool * filter_leaf;
  test_open_description: filter_leaf -> open_description -> bool * filter_leaf;
  test_pat: filter_leaf -> pattern -> bool * filter_leaf;
  test_payload: filter_leaf -> payload -> bool * filter_leaf;
  test_signature: filter_leaf -> signature -> bool * filter_leaf;
  test_signature_item: filter_leaf -> signature_item -> bool * filter_leaf;
  test_structure: filter_leaf -> structure -> bool * filter_leaf;
  test_structure_item: filter_leaf -> structure_item -> bool * filter_leaf;
  test_typ: filter_leaf -> core_type -> bool * filter_leaf;
  test_type_declaration: filter_leaf -> type_declaration -> bool * filter_leaf;
  test_type_extension: filter_leaf -> type_extension -> bool * filter_leaf;
  test_type_kind: filter_leaf -> type_kind -> bool * filter_leaf;
  test_value_binding: filter_leaf -> value_binding -> bool * filter_leaf;
  test_value_description: filter_leaf -> value_description -> bool * filter_leaf;
  test_with_constraint: filter_leaf -> with_constraint -> bool * filter_leaf;
}

module Lens =
struct
  type ('container, 'elt) t = {
    get: 'container -> 'elt;
    set: 'elt -> 'container -> 'container;
  }

  let test_attribute = {
    get = (fun x -> x.test_attribute);
    set = (fun v x -> { x with test_attribute = v });
  }
  let test_attributes = {
    get = (fun x -> x.test_attributes);
    set = (fun v x -> { x with test_attributes = v });
  }
  let test_case = {
    get = (fun x -> x.test_case);
    set = (fun v x -> { x with test_case = v });
  }
  let test_cases = {
    get = (fun x -> x.test_cases);
    set = (fun v x -> { x with test_cases = v });
  }
  let test_class_declaration = {
    get = (fun x -> x.test_class_declaration);
    set = (fun v x -> { x with test_class_declaration = v });
  }
  let test_class_description = {
    get = (fun x -> x.test_class_description);
    set = (fun v x -> { x with test_class_description = v });
  }
  let test_class_expr = {
    get = (fun x -> x.test_class_expr);
    set = (fun v x -> { x with test_class_expr = v });
  }
  let test_class_field = {
    get = (fun x -> x.test_class_field);
    set = (fun v x -> { x with test_class_field = v });
  }
  let test_class_signature = {
    get = (fun x -> x.test_class_signature);
    set = (fun v x -> { x with test_class_signature = v });
  }
  let test_class_structure = {
    get = (fun x -> x.test_class_structure);
    set = (fun v x -> { x with test_class_structure = v });
  }
  let test_class_type = {
    get = (fun x -> x.test_class_type);
    set = (fun v x -> { x with test_class_type = v });
  }
  let test_class_type_declaration = {
    get = (fun x -> x.test_class_type_declaration);
    set = (fun v x -> { x with test_class_type_declaration = v });
  }
  let test_class_type_field = {
    get = (fun x -> x.test_class_type_field);
    set = (fun v x -> { x with test_class_type_field = v });
  }
  let test_constructor_declaration = {
    get = (fun x -> x.test_constructor_declaration);
    set = (fun v x -> { x with test_constructor_declaration = v });
  }
  let test_expr = {
    get = (fun x -> x.test_expr);
    set = (fun v x -> { x with test_expr = v });
  }
  let test_extension = {
    get = (fun x -> x.test_extension);
    set = (fun v x -> { x with test_extension = v });
  }
  let test_extension_constructor = {
    get = (fun x -> x.test_extension_constructor);
    set = (fun v x -> { x with test_extension_constructor = v });
  }
  let test_include_declaration = {
    get = (fun x -> x.test_include_declaration);
    set = (fun v x -> { x with test_include_declaration = v });
  }
  let test_include_description = {
    get = (fun x -> x.test_include_description);
    set = (fun v x -> { x with test_include_description = v });
  }
  let test_label_declaration = {
    get = (fun x -> x.test_label_declaration);
    set = (fun v x -> { x with test_label_declaration = v });
  }
  let test_location = {
    get = (fun x -> x.test_location);
    set = (fun v x -> { x with test_location = v });
  }
  let test_module_binding = {
    get = (fun x -> x.test_module_binding);
    set = (fun v x -> { x with test_module_binding = v });
  }
  let test_module_declaration = {
    get = (fun x -> x.test_module_declaration);
    set = (fun v x -> { x with test_module_declaration = v });
  }
  let test_module_expr = {
    get = (fun x -> x.test_module_expr);
    set = (fun v x -> { x with test_module_expr = v });
  }
  let test_module_type = {
    get = (fun x -> x.test_module_type);
    set = (fun v x -> { x with test_module_type = v });
  }
  let test_module_type_declaration = {
    get = (fun x -> x.test_module_type_declaration);
    set = (fun v x -> { x with test_module_type_declaration = v });
  }
  let test_open_description = {
    get = (fun x -> x.test_open_description);
    set = (fun v x -> { x with test_open_description = v });
  }
  let test_pat = {
    get = (fun x -> x.test_pat);
    set = (fun v x -> { x with test_pat = v });
  }
  let test_payload = {
    get = (fun x -> x.test_payload);
    set = (fun v x -> { x with test_payload = v });
  }
  let test_signature = {
    get = (fun x -> x.test_signature);
    set = (fun v x -> { x with test_signature = v });
  }
  let test_signature_item = {
    get = (fun x -> x.test_signature_item);
    set = (fun v x -> { x with test_signature_item = v });
  }
  let test_structure = {
    get = (fun x -> x.test_structure);
    set = (fun v x -> { x with test_structure = v });
  }
  let test_structure_item = {
    get = (fun x -> x.test_structure_item);
    set = (fun v x -> { x with test_structure_item = v });
  }
  let test_typ = {
    get = (fun x -> x.test_typ);
    set = (fun v x -> { x with test_typ = v });
  }
  let test_type_declaration = {
    get = (fun x -> x.test_type_declaration);
    set = (fun v x -> { x with test_type_declaration = v });
  }
  let test_type_extension = {
    get = (fun x -> x.test_type_extension);
    set = (fun v x -> { x with test_type_extension = v });
  }
  let test_type_kind = {
    get = (fun x -> x.test_type_kind);
    set = (fun v x -> { x with test_type_kind = v });
  }
  let test_value_binding = {
    get = (fun x -> x.test_value_binding);
    set = (fun v x -> { x with test_value_binding = v });
  }
  let test_value_description = {
    get = (fun x -> x.test_value_description);
    set = (fun v x -> { x with test_value_description = v });
  }
  let test_with_constraint = {
    get = (fun x -> x.test_with_constraint);
    set = (fun v x -> { x with test_with_constraint = v });
  }
end

type t =
  | Test of filter_leaf
  | And of t*t
  | Or of t*t

let test_ t = Test t
let and_ t1 t2 = And (t1, t2)
let or_ t1 t2 = Or (t1, t2)

let all = {
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

let nothing = {
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

let ( &@ ) = and_
let ( |@ ) = or_

let rec apply_filter self getter node =
  let binop l r op combinator =
    let res_l, self_l = apply_filter l getter node
    and res_r, self_r = apply_filter r getter node
    in
    op res_l res_r, combinator self_l self_r
  in
  match self with
  | Test t -> let res, new_self = (getter t) t node in res, Test new_self
  | And (l, r) ->
    binop l r (&&) and_
  | Or (l, r) ->
    binop l r (||) or_

let rec limit_range condition patch = let open Ast_mapper in
  (* TODO: reprendre pour ne pas pourrir l'open recursion ? *)
  let choose_apply
      getter
      trans
      default_trans
      parent_mapper
      node
    =
    match apply_filter condition getter node with
    | (true, sub) -> trans (limit_range sub patch) node
    | (false, sub) -> default_trans (limit_range sub patch) node
  in {
    attribute = choose_apply
        Lens.test_attribute.Lens.get patch.attribute default_mapper.attribute;
    attributes = choose_apply
        Lens.test_attributes.Lens.get patch.attributes default_mapper.attributes;
    case = choose_apply
        Lens.test_case.Lens.get patch.case default_mapper.case;
    cases = choose_apply
        Lens.test_cases.Lens.get patch.cases default_mapper.cases;
    class_declaration = choose_apply
        Lens.test_class_declaration.Lens.get patch.class_declaration default_mapper.class_declaration;
    class_description = choose_apply
        Lens.test_class_description.Lens.get patch.class_description default_mapper.class_description;
    class_expr = choose_apply
        Lens.test_class_expr.Lens.get patch.class_expr default_mapper.class_expr;
    class_field = choose_apply
        Lens.test_class_field.Lens.get patch.class_field default_mapper.class_field;
    class_signature = choose_apply
        Lens.test_class_signature.Lens.get patch.class_signature default_mapper.class_signature;
    class_structure = choose_apply
        Lens.test_class_structure.Lens.get patch.class_structure default_mapper.class_structure;
    class_type = choose_apply
        Lens.test_class_type.Lens.get patch.class_type default_mapper.class_type;
    class_type_declaration = choose_apply
        Lens.test_class_type_declaration.Lens.get patch.class_type_declaration default_mapper.class_type_declaration;
    class_type_field = choose_apply
        Lens.test_class_type_field.Lens.get patch.class_type_field default_mapper.class_type_field;
    constructor_declaration = choose_apply
        Lens.test_constructor_declaration.Lens.get patch.constructor_declaration default_mapper.constructor_declaration;
    expr = choose_apply
        Lens.test_expr.Lens.get patch.expr default_mapper.expr;
    extension = choose_apply
        Lens.test_extension.Lens.get patch.extension default_mapper.extension;
    extension_constructor = choose_apply
        Lens.test_extension_constructor.Lens.get patch.extension_constructor default_mapper.extension_constructor;
    include_declaration = choose_apply
        Lens.test_include_declaration.Lens.get patch.include_declaration default_mapper.include_declaration;
    include_description = choose_apply
        Lens.test_include_description.Lens.get patch.include_description default_mapper.include_description;
    label_declaration = choose_apply
        Lens.test_label_declaration.Lens.get patch.label_declaration default_mapper.label_declaration;
    location = choose_apply
        Lens.test_location.Lens.get patch.location default_mapper.location;
    module_binding = choose_apply
        Lens.test_module_binding.Lens.get patch.module_binding default_mapper.module_binding;
    module_declaration = choose_apply
        Lens.test_module_declaration.Lens.get patch.module_declaration default_mapper.module_declaration;
    module_expr = choose_apply
        Lens.test_module_expr.Lens.get patch.module_expr default_mapper.module_expr;
    module_type = choose_apply
        Lens.test_module_type.Lens.get patch.module_type default_mapper.module_type;
    module_type_declaration = choose_apply
        Lens.test_module_type_declaration.Lens.get patch.module_type_declaration default_mapper.module_type_declaration;
    open_description = choose_apply
        Lens.test_open_description.Lens.get patch.open_description default_mapper.open_description;
    pat = choose_apply
        Lens.test_pat.Lens.get patch.pat default_mapper.pat;
    payload = choose_apply
        Lens.test_payload.Lens.get patch.payload default_mapper.payload;
    signature = choose_apply
        Lens.test_signature.Lens.get patch.signature default_mapper.signature;
    signature_item = choose_apply
        Lens.test_signature_item.Lens.get patch.signature_item default_mapper.signature_item;
    structure = choose_apply
        Lens.test_structure.Lens.get patch.structure default_mapper.structure;
    structure_item = choose_apply
        Lens.test_structure_item.Lens.get patch.structure_item default_mapper.structure_item;
    typ = choose_apply
        Lens.test_typ.Lens.get patch.typ default_mapper.typ;
    type_declaration = choose_apply
        Lens.test_type_declaration.Lens.get patch.type_declaration default_mapper.type_declaration;
    type_extension = choose_apply
        Lens.test_type_extension.Lens.get patch.type_extension default_mapper.type_extension;
    type_kind = choose_apply
        Lens.test_type_kind.Lens.get patch.type_kind default_mapper.type_kind;
    value_binding = choose_apply
        Lens.test_value_binding.Lens.get patch.value_binding default_mapper.value_binding;
    value_description = choose_apply
        Lens.test_value_description.Lens.get patch.value_description default_mapper.value_description;
    with_constraint = choose_apply
        Lens.test_with_constraint.Lens.get patch.with_constraint default_mapper.with_constraint;
  }
