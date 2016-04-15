open Check_types
open Info

type source_kind = Source | Interface | Cmt | Config

let string_of_source_kind = function
  | Source -> "ml files"
  | Interface -> "mli files"
  | Cmt -> "cmt* files"
  | Config -> "configuration file"

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
  Format.eprintf "Scanning %S in project %S...\n%!"
    (string_of_source_kind kind) path;
  let found_files =
    let files = ref [] in
    iter_files (fun file ->
        if (kind = Source && Filename.check_suffix file "ml") ||
           (kind = Interface && Filename.check_suffix file "mli")  ||
           (kind = Cmt && Filename.check_suffix file "cmt") ||
           (kind = Config && Filename.check_suffix file "conf")
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

let scan_config path =
  let config = scan_project ~kind:Config path in
  match config with
  | [] -> Configuration.default   (* No configuration file, take the default. *)
  | [ config_file ] -> Configuration.read_config config_file
  | configs -> (* TODO what to do when we define many configs file ? *)
    Configuration.default

let filter_checks filters checks =
  let flags = Checks.parse_options false filters in
  List.fold_left2
    (fun acc flag check -> if flag then check :: acc else acc)
    [] (Array.to_list flags) checks

let scan ~filters ~json_file ~txt_file path =
  (* Initializing states (config, reports set, etc. *)
  let config = scan_config path in
  (* XXX TODO:
     Now that we read config, we need to remove all analyses which don't appear
     in the config file. *)
  let reports : Reports.t = Reports.empty in

  (* XXX TODO : don't forget to read config file too ! *)
  (* Getting all warnings available in ocp-lint *)
  let checks = filter_checks filters Checks.checks in

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

  Printf.eprintf "Starting analyses...\n%!";

  (* Global Checks *)
  List.iter
    (fun check -> check.global_run config reports sources)
    (Checks.global_checks checks);

  (* Checks on each source files. *)
  List.iter (fun check ->
      List.iter (fun ast -> check.source_run config reports ast) asts_ml)
    (Checks.source_checks checks);

  (* Start analyses on Cmt. *)
  List.iter (fun check ->
      List.iter (fun cmt -> check.cmt_run config reports cmt) cmts)
    (Checks.cmt_checks checks);

  match txt_file, json_file with
    | None, None -> Txt.print reports
    | None, Some json_file -> Json.json reports json_file
    | Some txt_file, None -> Txt.txt reports txt_file
    | Some json_file, Some txt_file ->
      Json.json reports json_file;
      Txt.txt reports txt_file

let list_warnings () =
  Format.eprintf "List of warnings :\n";
  List.iter (fun ((id, check) as c)->
      Format.eprintf "%4d %s\n" id (get_info c).name)
    Checks.checks;

  Format.eprintf "\n";
  let all =
    String.concat " " (List.rev_map string_of_int (Checks.letter 'a')) in

  let code_set =
    String.concat " " (List.rev_map string_of_int (Checks.letter 'c')) in

  let interface_set =
    String.concat " " (List.rev_map string_of_int (Checks.letter 'i')) in

  let typo_set =
    String.concat " " (List.rev_map string_of_int (Checks.letter 't')) in

  Format.eprintf "   A All warnings\n";
  Format.eprintf "     Set of warnings %s\n" all;

  Format.eprintf "   C All warnings touching the code\n";
  Format.eprintf "     Set of warnings %s\n" code_set;

  Format.eprintf "   I All warnings touching the interface\n";
  Format.eprintf "     Set of warnings %s\n" interface_set;

  Format.eprintf "   T All warnings touching the typography\n";
  Format.eprintf "     Set of warnings %s\n" typo_set;
