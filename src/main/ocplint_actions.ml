let (analyses : (module Analyse.CHECK) list) = [
  Code_identifier_length.check;
  Code_length.check
]

let scan_project path = (* todo *)
  Format.eprintf "Scanning project %s...\n%!" path;
  let found_files = ["toto.cmt"] in
  Format.eprintf "Found '%d' file(s)\n%!" (List.length found_files);
  found_files

let scan_files path =

  let files = scan_project path in
  List.map Cmt_format.read_cmt files

let scan path =
  let open Cmt_format in
  let cmts = scan_files path in
  Printf.eprintf "Starting analyses...\n\n%!";

  List.fold_left (fun reports chk ->
      (* Getting all analyse as a Typedtree IteratorArgument *)
      let module A = (val chk : Analyse.CHECK) in
      let module IA = (val A.analyse : TypedtreeIter.IteratorArgument) in
      let module Check = (TypedtreeIter.MakeIterator(IA)) in

      (* Itering on files *)
      List.fold_left (fun reports cmt ->
          match cmt.cmt_annots with
          | Implementation str ->
            Check.iter_structure str; reports
          | Interface interface ->
            Check.iter_signature interface; reports
          | Packed (tsig, strl) -> assert false (* todo *)
          | Partial_implementation bin_partial_arr
          | Partial_interface  bin_partial_arr ->
            Array.fold_left (fun reports bin_partial ->
                match bin_partial with
                | Partial_structure str ->
                  Check.iter_structure str; reports
                | Partial_structure_item str_item ->
                  Check.iter_structure_item str_item; reports
                | Partial_expression expr ->
                  Check.iter_expression expr; reports
                | Partial_pattern pat ->
                  Check.iter_pattern pat; reports
                | Partial_class_expr cl_expr ->
                  Check.iter_class_expr cl_expr; reports
                | Partial_signature sign ->
                  Check.iter_signature sign; reports
                | Partial_signature_item sign_item ->
                  Check.iter_signature_item sign_item; reports
                | Partial_module_type mod_type ->
                  Check.iter_module_type mod_type; reports)
              reports  bin_partial_arr)
        reports cmts)
    [] analyses
