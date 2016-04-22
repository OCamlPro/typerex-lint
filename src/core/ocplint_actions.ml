
type source_kind = All | Source | Interface | Cmt | Config

let string_of_source_kind = function
  | All -> "all files"
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
        if (kind = All) ||
           (kind = Source && Filename.check_suffix file "ml") ||
           (kind = Interface && Filename.check_suffix file "mli")  ||
           (kind = Cmt && Filename.check_suffix file "cmt") ||
           (kind = Config && Filename.check_suffix file "conf")
        then
          files := (Filename.concat path file) :: !files) path;
    !files in
  Format.eprintf "Found '%d' file(s)\n%!" (List.length found_files);
  found_files

let scan_all path = scan_project ~kind:All path

let scan_sources ?(kind=Source) path = scan_project ~kind path

let scan_cmts path =
  let files = scan_project ~kind:Cmt path in
  List.map (fun file -> lazy (Cmt_format.read_cmt file)) files

let filter_plugins filters =
  (* TODO: xxx filter options in command-line or configuration file *)
  Plugin.plugins

let parse_source source =
  Pparse.parse_implementation ~tool_name:"" Format.err_formatter source

let parse_interf source =
  Pparse.parse_interface ~tool_name:"" Format.err_formatter source

let scan ~filters path =
  (* XXX TODO : don't forget to read config file too ! *)
  (* let plugins = filter_plugins filters in *)

  let all = scan_all path in
  (* All inputs for each analyze *)
  let mls, mlis = scan_sources path, scan_sources ~kind:Interface path in

  let cmts = scan_cmts path in
  let asts_ml, asts_mli =
    List.map (fun file -> lazy (parse_source file)) mls,
    List.map (fun file -> lazy (parse_interf file)) mlis in

  Printf.eprintf "Starting analyses...\n%!";

  Parallel_engine.lint all mls mlis asts_ml asts_mli cmts
