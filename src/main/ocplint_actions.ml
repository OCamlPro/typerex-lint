open Check_types
open Info

let global_checks : Check_types.global_check list = [
  Interface_missing.check;
  Code_length.check
]

let sources_checks : Check_types.source_check list = [
  Code_identifier_length.check;
]

let cmt_checks : Check_types.cmt_check list = [
  Test_cmt.check
]

let iter_files ?(recdir=true) f dirname =
  let rec iter dirname dir =
    let files = Sys.readdir (Filename.concat dirname dir) in
    Array.iter (fun file ->
        let file = Filename.concat dir file in
        if Sys.is_directory (Filename.concat dirname file) then begin
          if recdir then iter dirname file
        end else
          f file)
      files
  in
  iter dirname ""

let scan_project ?(kind=Source) path = (* todo *)
  Format.eprintf "Scanning %S in project %S...\n%!" (kind_of_string kind) path;
  let found_files =
    let files = ref [] in
    iter_files (fun file ->
        if (kind = Source && Filename.check_suffix file "ml") ||
           (kind = Interface && Filename.check_suffix file "mli")  ||
           (kind = Cmt && Filename.check_suffix file "cmt")
        then
          files := (Filename.concat path file) :: !files) path;
    !files in
  Format.eprintf "Found '%d' file(s)\n%!" (List.length found_files);
  found_files

let scan_files ?(kind=Source) path =
  scan_project ~kind path

let scan_cmts path =
  let files = scan_project ~kind:Cmt path in
  List.map Cmt_format.read_cmt files

let scan path =
  (* All inputs for each analyze *)
  let sources : string list = scan_files path in
  let cmts : Cmt_format.cmt_infos list = scan_cmts path in
  let asts_mli, asts_ml =
    List.fold_left (fun (mli, ml) source ->
        let tool_name = Ast_mapper.tool_name () in
        if Filename.check_suffix source "ml" then
          mli, parse_source ~tool_name source :: ml
        else
          parse_interf ~tool_name source :: mli, ml)
      ([], []) sources in

  (* Initializing states (config, reports set, etc. *)
  let config = Configuration.default in
  let reports : Reports.t = Reports.empty in

  Printf.eprintf "Starting analyses...\n%!";

  (* Global Checks *)
  List.iter (fun check -> check.global_run config reports sources) global_checks;

  (* Checks on each source files. *)
  List.iter (fun check ->
      List.iter (fun ast -> check.source_run config reports ast) asts_ml)
    sources_checks;

  (* Start analyses on Cmt. *)
  List.iter (fun check ->
      List.iter (fun cmt -> check.cmt_run config reports cmt) cmts)
    cmt_checks;

  Reports.print reports
