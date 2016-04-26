open SimpleConfig

let ignored_files = Globals.Config.create_option
    ["ignored_files"]
    ~short_help:"Module to ignore during the lint."
    ["Module to ignore during the lint."]
    ~level:0
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let iter_files ?(recdir=true) apply dirname =
  let rec iter dirname dir =
    let files = Sys.readdir (Filename.concat dirname dir) in
    Array.iter (fun file ->
        let file = Filename.concat dir file in
        if Sys.is_directory (Filename.concat dirname file) then begin
          if recdir then iter dirname file
        end else
          apply file)
      files
  in
  iter dirname ""

let scan_project path = (* todo *)
  Format.printf "Scanning files in project %S...\n%!" path;
  let found_files =
    let files = ref [] in
    iter_files (fun file ->
        files := (Filename.concat path file) :: !files) path;
    !files in
  Format.printf "Found '%d' file(s)\n%!" (List.length found_files);
  found_files

let filter_plugins filters =
  (* TODO: xxx filter options in command-line or configuration file *)
  Globals.plugins

let filter_modules sources filters =
  List.filter (fun source ->
      not (List.exists (fun ignored -> ignored = source) filters)) sources

let parse_source source =
  try
    Some
      (Pparse.parse_implementation ~tool_name:"" Format.err_formatter  source)
  with Syntaxerr.Error _ ->
    Printf.printf "Cannot lint %S.\n" source;
    None

let parse_interf source =
  try
    Some (Pparse.parse_interface ~tool_name:"" Format.err_formatter source)
  with Syntaxerr.Error _ ->
    Printf.printf "Cannont lint %S.\n" source;
    None

let is_source file = Filename.check_suffix file "ml"
let is_interface file = Filename.check_suffix file "mli"
let is_cmt file = Filename.check_suffix file "cmt"
let is_cmt file = Filename.check_suffix file "cmt"

let scan ~filters path =
  (* XXX TODO : don't forget to read config file too ! *)
  (* let plugins = filter_plugins filters in *)

  let all = filter_modules (scan_project path) !!ignored_files in

  (* All inputs for each analyze *)
  let mls = List.filter (fun file -> is_source file) all in
  let mlis = List.filter (fun file -> is_interface file) all in

  let cmts =
    let files = List.filter (fun file -> is_cmt file) all in
    List.map (fun file -> lazy (Cmt_format.read_cmt file)) files in

  let asts_ml, asts_mli =
    List.map (fun file -> lazy (parse_source file)) mls,
    List.map (fun file -> lazy (parse_interf file)) mlis in

  Format.printf "Starting analyses...\n%!";

  Parallel_engine.lint all mls mlis asts_ml asts_mli cmts
