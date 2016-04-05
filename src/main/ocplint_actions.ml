open Check_types
open Info

let global_checks : Check_types.global_check list = [
  Interface_missing.check;
  Code_length.check
]

let (analyses : Check_types.source_check list) = [
  Code_identifier_length.check;
]

let iter_files ?(recdir=true) f dirname =
  let rec iter dirname dir =
    let files = Sys.readdir (Filename.concat dirname dir) in
    Array.iter (fun file ->
        let file = Filename.concat dir file in
        if Sys.is_directory (Filename.concat dirname file) then begin
          if recdir then iter dirname file
        end else
          f file
      ) files
  in
  iter dirname ""

let scan_project ?(kind=Source) path = (* todo *)
  Format.eprintf "Scanning %S in project %S...\n%!" (kind_of_string kind) path;
  let found_files =
    let files = ref [] in
    iter_files (fun file ->
        if (kind = Source &&
            (Filename.check_suffix file "ml"
             || Filename.check_suffix file "mli"))  ||
           (kind = Cmt && Filename.check_suffix file "cmt")
        then
          files := (Filename.concat path file) :: !files) path;
    !files in
  Format.eprintf "Found '%d' file(s)\n%!" (List.length found_files);
  found_files

let scan_files ?(kind=Source) path =
  scan_project ~kind path

(* let scan_cmts path = *)
(*   let files = scan_project ~kind:Cmt path in *)
(*   List.map Cmt_format.read_cmt files *)

(* let scan_checks () = *)
(*   let open Info in *)
(*   List.iter (fun check -> *)
(*       let info = check.info in *)
(*       Format.eprintf " [%s]: %s\n   %s\n%!" *)
(*         (cat_to_string info.cat) *)
(*         (info.name) *)
(*         (info.details)) analyses *)

let scan path =
  let sources : string list = scan_files path in
  let asts_mli, asts_ml =
    List.fold_left (fun (mli, ml) source ->
        let tool_name = Ast_mapper.tool_name () in
        if Filename.check_suffix source "ml" then
          mli, parse_source ~tool_name source :: ml
        else
          parse_interf ~tool_name source :: mli, ml)
      ([], []) sources in
  (* let asts = *)
  (*   List.map (fun source -> ) sources in *)

  (* let interfaces = scan_files ~kind:Interface path in *)
  let config = Configuration.default in
  let reports : Reports.t = Reports.empty in

  Printf.eprintf "Starting analyses...\n%!";

  (* (\* Global Checks *\) *)
  List.iter (fun check ->
      (* Printf.eprintf "  --- [%s] %s ---\n%!" *)
      (*   (cat_to_string check.global_info.cat) *)
      (*   (check.global_info.name); *)
      check.global_run config reports sources)
    global_checks;


  (* Checks on each source files *)
  List.iter (fun check ->
      List.iter (fun ast ->
          (* Printf.eprintf "  --- [%s] %s ---\n%!" *)
          (*   (cat_to_string check.source_info.cat) *)
          (*   (check.source_info.name); *)
          check.source_run config reports ast)
        asts_ml)
    analyses;

  Reports.print reports









  (* List.fold_left (fun reports chk -> *)
  (*     (\* Getting all analyse as a Typedtree IteratorArgument *\) *)
  (*     let module A = (val chk : Analyse.ANALYSE) in *)
  (*     (\* let module AA = (A(T)) in *\) *)
  (*     let check = A.check in *)
  (*     (\* let module Check = (TypedtreeIter.MakeIterator(IA)) in *\) *)
  (*     (\* let config = AA.config in *\) *)
  (*     Printf.eprintf "CONFIG %s\n%!" config; *)
    (*   List.fold_left (fun reports cmt -> *)
    (*       match cmt.cmt_annots with *)
    (*       | Implementation str -> *)
    (*         Check.iter_structure str; reports *)
    (*       | Interface interface -> *)
    (*         Check.iter_signature interface; reports *)
    (*       | Packed (tsig, strl) -> assert false (\* todo *\) *)
    (*       | Partial_implementation bin_partial_arr *)
    (*       | Partial_interface  bin_partial_arr -> *)
    (*         Array.fold_left (fun reports bin_partial -> *)
    (*             match bin_partial with *)
    (*             | Partial_structure str -> *)
    (*               Check.iter_structure str; reports *)
    (*             | Partial_structure_item str_item -> *)
    (*               Check.iter_structure_item str_item; reports *)
    (*             | Partial_expression expr -> *)
    (*               Check.iter_expression expr; reports *)
    (*             | Partial_pattern pat -> *)
    (*               Check.iter_pattern pat; reports *)
    (*             | Partial_class_expr cl_expr -> *)
    (*               Check.iter_class_expr cl_expr; reports *)
    (*             | Partial_signature sign -> *)
    (*               Check.iter_signature sign; reports *)
    (*             | Partial_signature_item sign_item -> *)
    (*               Check.iter_signature_item sign_item; reports *)
    (*             | Partial_module_type mod_type -> *)
    (*               Check.iter_module_type mod_type; reports) *)
    (*           reports bin_partial_arr) *)
    (*     reports cmts) *)
    (* [] analyses *)
