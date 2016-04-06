type source_check = {
  source_run :
    Configuration.t ->
    Reports.t ->
    Parsetree.structure ->
    unit;
  source_info : Info.t;
}

type global_check = {
  global_run :
    Configuration.t ->
    Reports.t ->
    string list ->
    unit;
  global_info : Info.t;
}

type cmt_check = {
  cmt_run :
    Configuration.t ->
    Reports.t ->
    Cmt_format.cmt_infos ->
    unit;
  cmt_info : Info.t;
}

let parse_source ~tool_name source =
  Pparse.parse_implementation ~tool_name Format.err_formatter source

let parse_interf ~tool_name interface =
  Pparse.parse_interface ~tool_name Format.err_formatter interface

let iter_cmt iterator cmt =
  let open Cmt_format in
  let module IA = (val iterator : TypedtreeIter.IteratorArgument) in
  let module I = (TypedtreeIter.MakeIterator(IA)) in
  match cmt.cmt_annots with
  | Implementation str ->
    I.iter_structure str
  | Interface interface ->
    I.iter_signature interface
  | Packed (tsig, strl) -> assert false (* todo *)
  | Partial_implementation bin_partial_arr
  | Partial_interface bin_partial_arr ->
    Array.iter (fun bin_partial ->
        match bin_partial with
        | Partial_structure str ->
          I.iter_structure str
        | Partial_structure_item str_item ->
          I.iter_structure_item str_item
        | Partial_expression expr ->
          I.iter_expression expr
        | Partial_pattern pat ->
          I.iter_pattern pat
        | Partial_class_expr cl_expr ->
          I.iter_class_expr cl_expr
        | Partial_signature sign ->
          I.iter_signature sign
        | Partial_signature_item sign_item ->
          I.iter_signature_item sign_item
        | Partial_module_type mod_type ->
          I.iter_module_type mod_type)
      bin_partial_arr
