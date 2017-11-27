(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

let (!!) = SimpleConfig.(!!)
open Lint_warning

(* This structure is used to be able to reuse the parsetree, typedtree, etc
   instead of regenerating it each time *)
type file_t = {
  filename: string;
  ast: (Parsetree.structure option) lazy_t;
  asti: (Parsetree.signature option) lazy_t;
  cmt: ((Cmt_format.cmt_infos) lazy_t) option;
}

let ignored = Lint_globals.LintConfig.create_option
    ["ignore"]
    "Module to ignore during the lint."
    "Module to ignore during the lint."
    0
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let db_persistence = Lint_globals.LintConfig.create_option
    ["db_persistence"]
    "Time before erasing cached results (in days)."
    "Time before erasing cached results (in days)."
    0
    SimpleConfig.int_option
    1

let jobs = Lint_globals.LintConfig.create_option
    ["jobs"]
    "Number of parallel jobs"
    "Number of parallel jobs"
    0
    SimpleConfig.int_option
    4

let file_config_dep = ref []

let scan_project path = (* todo *)
  Format.printf "Scanning files in project %S...\n%!" path;
  let found_files =
    let files = ref [] in
    Lint_utils.iter_files (fun file ->
        let file = if path = Filename.current_dir_name then
            file
          else Filename.concat path file in
        files := file :: !files) path;
    !files in
  Format.printf "Found '%d' file(s)\n%!" (List.length found_files);
  found_files

let filter_plugins plugins =
  let activated_plugins = Hashtbl.create 42 in
  Lint_plugin.iter_plugins (fun plugin checks ->
      let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
      let plugin_short_name = Plugin.short_name in
      let plugin_opt_names = [ plugin_short_name; "enabled" ] in
      let plugin_opt_value =
        Lint_globals.LintConfig.get_option_value plugin_opt_names in
      (* if the plugin is disable, don't try to add any linter attached to it *)
      if bool_of_string plugin_opt_value then begin
        Lint_map.iter (fun cname lint ->
            let lint_opt_names = [ plugin_short_name; cname; "enabled" ] in
            let lint_opt_value =
              Lint_globals.LintConfig.get_option_value lint_opt_names in
            (* if the linter is disable, don't try to use it. *)
            if bool_of_string lint_opt_value then begin
              let old_lints =
                try Hashtbl.find activated_plugins plugin
                with Not_found -> Lint_map.empty in
              Hashtbl.replace activated_plugins plugin
                (Lint_map.add cname lint old_lints)
            end)
          checks
      end    )
    plugins;
  activated_plugins

let is_same_file root file1 file2 =
  let file1 = Lint_utils.relative_path root file1 in
  let file2 = Lint_utils.relative_path root file2 in
  file1 = file2

let is_ignored root file ignored =
  List.exists (fun source ->
      try
        if Sys.is_directory source then
          Lint_utils.is_in_path root file source
        else is_same_file root file source
      with Sys_error _ -> is_same_file root file source)
    ignored

let filter_modules sources ignored =
  let root = !Lint_db.DefaultDB.root in
  let db_dir = Filename.concat root Lint_globals.olint_dirname in
  List.filter (fun source ->
      not (is_ignored root source ignored) &&
      not (Lint_utils.is_in_path root source db_dir))
    sources

let parse_source source =
  let tool_name = Ast_mapper.tool_name () in
  try
    Some
      (Pparse.parse_implementation ~tool_name Format.err_formatter  source)
  with Syntaxerr.Error _ ->
    raise Lint_plugin_error.(Plugin_error (Syntax_error source))

let parse_interf source =
  let tool_name = Ast_mapper.tool_name () in
  try
    Some (Pparse.parse_interface ~tool_name Format.err_formatter source)
  with Syntaxerr.Error _ ->
    raise Lint_plugin_error.(Plugin_error (Syntax_error source))

let is_source file = Filename.check_suffix file ".ml"
let is_interface file = Filename.check_suffix file ".mli"
let is_cmt file = Filename.check_suffix file ".cmt"
let is_cmt file = Filename.check_suffix file ".cmt"
let is_cmxs file = Filename.check_suffix file ".cmxs"

let ( // ) = Filename.concat

let load_installed_plugins () =
  let good_plugins = ref [] in
  let exe_name_path = Sys.executable_name in
  let exe_name =
    try
      Filename.chop_extension (Filename.basename exe_name_path)
    with Invalid_argument err -> Filename.basename exe_name_path in
  if exe_name = "ocp-lint-minimal" then
    begin
      let bin_dir = Filename.dirname exe_name_path in
      let lib_dir = Filename.concat bin_dir "../lib" in
      let plugins_dir = Filename.concat lib_dir "ocp-lint-plugins" in
      if Sys.file_exists plugins_dir && Sys.is_directory plugins_dir then
        begin
          let plugins = Sys.readdir plugins_dir in
          Printf.eprintf "Trying to load %d plugin(s)...\n%!"
            (Array.length plugins);
          Array.iter (fun plugin ->
              try
                Fl_dynload.load_packages [ plugin ];
                good_plugins := plugin :: !good_plugins
              with Dynlink.Error err ->
                Printf.eprintf "Can not load %S plugin: %s\n%!"
                  plugin (Dynlink.error_message err)
            ) plugins;
        end;
    end;
  !good_plugins

let rec load_plugins list =
  List.fold_left (fun acc file ->
        try
          if Sys.is_directory file then begin
            let files = ref [] in
            Lint_utils.iter_files (fun f ->
                files := (file // f) :: !files) file;
            load_plugins (List.filter is_cmxs !files) @ acc
          end
          else if Filename.check_suffix file "cmxs" then
            (Dynlink.loadfile file;
             file :: acc)
          else
            (Printf.eprintf "Cannot load %S\n%!" file;
             acc)
        with
        | Sys_error _ ->
          Printf.eprintf "%S: No such file or directory.\n%!" file;
          acc
        | Lint_plugin_error.Plugin_error err ->
          let str = Lint_plugin_error.to_string err in
          Printf.eprintf "Error while dynlinking : %s \n%!" str;
          acc)
      [] list

let init_olint_dir () =
  FileGen.safe_mkdir (FileGen.of_string Lint_globals.olint_dirname)

let init_db_in_tmp () =
  let tmp_dir = Lint_utils.mk_temp_dir "olint" in
  let olint_dir = Filename.concat tmp_dir Lint_globals.olint_dirname in
  FileGen.safe_mkdir (FileGen.of_string olint_dir);
  Lint_db.DefaultDB.init (FileGen.of_string olint_dir);
  Some tmp_dir

let clean_db_in_tmp db_dir = match db_dir with
  | None -> ()
  | Some dir ->
    let root = dir in
    let olint_dir = Filename.concat root Lint_globals.olint_dirname in
    FileDir.remove_all (FileGen.of_string olint_dir);
    Unix.rmdir dir

let init_db no_db db_dir path = match db_dir with
  | Some dir ->
    let olint_dirname = Lint_globals.olint_dirname in
    let root_t =
      FileGen.concat (FileGen.of_string dir) (FileGen.of_string olint_dirname) in
    Lint_db.DefaultDB.init root_t;
    db_dir, no_db
  | None ->
    let path_t = FileGen.of_string path in
    let olint_dirname = Lint_globals.olint_dirname in
    try
      if not no_db then
        (let root_path_dir_t = Lint_utils.find_root path_t [olint_dirname] in
         let root_t =
           FileGen.concat root_path_dir_t (FileGen.of_string olint_dirname) in
         Lint_db.DefaultDB.init root_t;
         Some (FileGen.to_string root_path_dir_t), no_db)
      else
        let db_dir = init_db_in_tmp () in
        db_dir, true
    with Not_found ->
      let db_dir = init_db_in_tmp () in
      db_dir, true

let init_config path =
  let path_t = FileGen.of_string path in
  let config_file = Lint_globals.LintConfig.config_file_name in
  try
    let root_path_t = Lint_utils.find_root path_t [config_file] in
    let file_t = FileGen.concat root_path_t (FileGen.of_string config_file) in
    Lint_globals.LintConfig.init_config file_t;
  with Not_found -> ()

let init_config_file file =
  let file_t = FileGen.of_string file in
  Lint_globals.LintConfig.init_config file_t

let list_plugins fmt =
  let open Lint_warning_decl in
  let open Lint_warning_types in
  Lint_plugin.iter_plugins (fun plugin checks ->
      let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
      let status = if Plugin.enable then "enable" else "disable" in
      if Plugin.enable then
        Format.fprintf fmt "=== %s === (\027[32m%s\027[m)\n" Plugin.name status
      else
        Format.fprintf fmt "=== %s === (\027[31m%s\027[m)\n" Plugin.name status;
      Lint_map.iter (fun cname lint ->
          let module Linter = (val lint : Lint_types.LINT) in
          let status = if Linter.enable then "enable" else "disable" in
          if Linter.enable then
            Format.fprintf fmt "  ** %s (\027[32m%s\027[m)\n%!"
              Linter.name status
          else
            Format.fprintf fmt "  ** %s (\027[31m%s\027[m)\n%!"
              Linter.name status;
          WarningDeclaration.iter (fun wdecl ->
              Format.fprintf fmt "      Warning %d: %S.\n%!"
                wdecl.id wdecl.short_name)
            Linter.wdecls)
        checks;
      Format.fprintf fmt "\n%!")
    Lint_globals.plugins

let get_ignored pname cname =
  let opt =
    Lint_globals.LintConfig.create_option [pname; cname; "ignore"]  "" "" 0
      (SimpleConfig.list_option SimpleConfig.string_option)  [] in
  !!opt

let print_db dir =
  let db = Lint_db.DefaultDB.load dir in
  Lint_db.DefaultDB.print_debug db

let mk_file_t file cmt_option = {
  filename = file;
  ast = lazy (parse_source file);
  asti = lazy (parse_interf file);
  cmt = match cmt_option with
    | None -> None
    | Some cmt_file -> Some (lazy (Cmt_format.read_cmt cmt_file))
}

let from_input file_struct file_t pname cname version inputs =
  let file = file_t.filename in
  List.iter (fun input ->
      try
        match input with
        | Lint_input.InMl main when is_source file ->
          Lint_db.DefaultDB.add_entry file_struct pname cname version;
          main file
        | Lint_input.InStruct main when is_source file ->
          begin match Lazy.force file_t.ast with
            | Some ast ->
              Lint_db.DefaultDB.add_entry file_struct pname cname version;
              main ast
            | None -> assert false
          end
        | Lint_input.InMli main when is_interface file ->
          Lint_db.DefaultDB.add_entry file_struct pname cname version;
          main file
        | Lint_input.InInterf main when is_interface file ->
          begin match Lazy.force file_t.asti with
            | Some ast ->
              Lint_db.DefaultDB.add_entry file_struct pname cname version;
              main ast
            | None -> ()
          end
        | Lint_input.InCmt main ->
          begin
            match file_t.cmt with
            | None -> ()
            | Some cmt ->
              let cmt = Lazy.force cmt in
              Lint_db.DefaultDB.add_entry file_struct pname cname version;
              main cmt
          end
        | Lint_input.InAll main ->
          Lint_db.DefaultDB.add_entry file_struct pname cname version;
          main [file]
        | Lint_input.InTop main -> assert false (* TODO *)
        | Lint_input.InTokens main when is_source file || is_interface file ->
          begin try
              main file
            with exn -> ()
          end
        | _ -> ()
      with
      | Lint_db_error.Db_error err ->
        Lint_db.DefaultDB.add_entry file_struct pname cname version;
        Lint_db.DefaultDB.add_error file (Lint_db_types.Db_error err)
      | Lint_plugin_error.Plugin_error err ->
        Lint_db.DefaultDB.add_entry file_struct pname cname version;
        Lint_db.DefaultDB.add_error file (Lint_db_types.Plugin_error err)
      | exn ->
        Lint_db.DefaultDB.add_entry file_struct pname cname version;
        Lint_db.DefaultDB.add_error
          file (Lint_db_types.Ocplint_error (Printexc.to_string exn)))
    inputs

let lint_file ~verbose ~no_db ~db_dir ~severity ~file_struct =
  let file = file_struct.Lint_utils.name in
  ignore (init_db no_db db_dir file);
  Lint_db.DefaultDB.load_file file_struct;
  Lint_db.DefaultDB.cache ();
  let plugins = filter_plugins Lint_globals.plugins in
  let open Lint_warning_decl in
  let open Lint_warning_types in
  let file_t = mk_file_t file file_struct.Lint_utils.cmt in
  Lint_plugin.iter_plugins (fun plugin checks ->
      Lint_map.iter (fun cname lint ->
          let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
          let module Linter = (val lint : Lint_types.LINT) in
          let ignored = get_ignored Plugin.short_name cname in
          let version = Linter.version in
          if not (is_ignored !Lint_db.DefaultDB.root file ignored) &&
             not (Lint_db.DefaultDB.already_run
                    file_struct Plugin.short_name Linter.short_name version)
          then
            from_input
              file_struct
              file_t
              Plugin.short_name
              Linter.short_name
              version
              Linter.inputs)
        checks)
    plugins;
  if verbose then
    Lint_text.verbose_info Format.err_formatter Lint_db.DefaultDB.db;
  Lint_db.DefaultDB.save ()

let add_config_option args cfg =
  let found = ref false in
  Array.iteri (fun i arg ->
      if arg = "--load-config" then begin
        found := true;
        args.(i + 1) <- cfg
      end)
    args;
  !found

let purify_load_plugins args gd_plugins =
  if gd_plugins <> []
  then
    let str = String.concat ","  gd_plugins in
    Array.iteri (fun i arg ->
        if arg = "--load-plugins" then begin
          args.(i + 1) <- str
        end) args
  else
    Array.iteri (fun i arg ->
        if arg = "--load-plugins" then begin
          args.(i) <- "";
          args.(i + 1) <- ""
        end) args

let is_config_file =
  let split_path str = OcpString.split str '/' in
  let rec is_config_file = function
    | (dir1::root, dir2::file) when dir1 = dir2 -> is_config_file (root, file)
    | ([], _) -> true
    | ([ "" ], _) -> true
    | (_, _) -> false
  in
  fun cfg file ->
    is_config_file (split_path cfg, split_path file)

let get_config_deps config_map file =
  List.filter (fun cfg -> is_config_file cfg file) config_map

let run
    db_dir ins_plug gd_plugins master_config config_map temp_dir file_struct =
  let open Lint_parallel_engine in
  let open Lint_utils in
  let temp_file = Lint_utils.save_file_struct temp_dir file_struct in
  let configs = get_config_deps config_map file_struct.name in
  let tmp_config =
    Lint_globals.LintConfig.load_and_save master_config configs in
  let args = Array.copy Sys.argv in
  let found = ref false in
  Array.iteri (fun i arg ->
      if arg = "--path" then begin
        found := true;
        args.(i) <- "--file";
        args.(i + 1) <- temp_file
      end)
    args;
  purify_load_plugins args gd_plugins;
  let found_load_cfg = add_config_option args tmp_config in
  let args = (* if ocp-lint is called without --path argument *)
    if not !found then Array.to_list args @ ["--file"; temp_file]
    else Array.to_list args in
  let args = match db_dir with
    | Some dir -> args @ [ "--db-dir"; dir ]
    | None -> args in
  let args =
    if not found_load_cfg then args @ ["--load-config"; tmp_config]
    else args in
  let args = if ins_plug <> [] then
      args @ [ "--load-installed-plugins"; String.concat "," ins_plug ]
    else args in
  let pid =
    Unix.create_process
      (List.hd args)
      (Array.of_list (List.filter (fun elt -> elt <> "") args))
      Unix.stdin
      Unix.stdout
      Unix.stderr in
  mark_running file_struct.name pid;
  file_config_dep := (file_struct, (configs, tmp_config))::!file_config_dep

let rec config_map_rec curr acc =
  let files = Sys.readdir curr in
  Array.fold_left (fun acc file ->
      let curr_file =
        if curr <> "." then Filename.concat curr file else file in
      if file = ".ocplint" then curr :: acc
      else
      if Sys.file_exists curr_file && Sys.is_directory curr_file
      then config_map_rec curr_file acc
      else acc)
    acc files

let config_map path = config_map_rec path []

let clean_tmp_cfg tmp_configs master_cfg =
  List.iter (fun (_, (_, file)) ->
      Sys.remove file;
      try Sys.remove (file ^ ".old") with _ -> ())
    tmp_configs;
  Sys.remove master_cfg;
  try Sys.remove (master_cfg ^ ".old")
  with _ -> ()

let lint_sequential ~no_db ~db_dir ~severity ~pdetail ~pwarning
    ~perror ~output_web ~gd_plugins ~ins_plugins ~master_config ~path =
  let open Lint_parallel_engine in
  (* We filter the global ignored modules/files.  *)
  if Hashtbl.length Lint_globals.plugins = 0 then
    Printf.eprintf "There is no plugin loaded.\n%!"
  else
  if Hashtbl.length (filter_plugins Lint_globals.plugins) = 0 then
    Printf.eprintf
      "There is no plugin activated at the root of your project.\n%!"
  else
  let (db_dir, no_db) = init_db no_db db_dir path in
  Lint_db.DefaultDB.clean !!db_persistence;
  let sources = filter_modules (scan_project path) !!ignored in
  let config_map = config_map path in
  let cmts_infos, sources = Lint_utils.split_sources sources in
  let len = List.length sources in
  let tmp_file_dir = Lint_utils.mk_temp_dir "olintfile" in
  FileGen.safe_mkdir (FileGen.of_string tmp_file_dir);
  mark_waiting sources;
  let start_list = get_start_list (!!jobs - 1) sources in
  List.iter (fun file ->
      let file_struct =
        Lint_utils.mk_file_struct !Lint_db.DefaultDB.root file [] cmts_infos in
      run
        db_dir
        ins_plugins
        gd_plugins
        master_config
        config_map
        tmp_file_dir
        file_struct
    ) start_list;
  while waiting_file () do
    let processed_files = done_files len in
    if processed_files mod 100 = 0 then
      Printf.eprintf "Running analyses... %d / %d\r%!" processed_files len;
    let pid, _status = Unix.waitpid [] 0 in
    try
      mark_done pid;
      let file = find_next_waiting () in
      let file_struct =
        Lint_utils.mk_file_struct !Lint_db.DefaultDB.root file [] cmts_infos in
      run
        db_dir
        ins_plugins
        gd_plugins
        master_config
        config_map
        tmp_file_dir
        file_struct
    with Not_found -> ()
  done;
  FileDir.remove_all (FileGen.of_string tmp_file_dir);
  Printf.eprintf "\rRunning analyses... %d / %d" len len;
  Printf.eprintf "\nMergin database...%!";
  let sources =
    List.map (fun file ->
        Lint_utils.mk_file_struct !Lint_db.DefaultDB.root file [] cmts_infos
      ) sources in
  Lint_db.DefaultDB.merge sources;
  Printf.eprintf "\n%!";
  if pwarning then
    Lint_text.print
      Format.err_formatter
      master_config
      !file_config_dep
      severity
      path
      Lint_db.DefaultDB.db;
  if perror then
    Lint_text.print_error
      Format.err_formatter
      path
      Lint_db.DefaultDB.db_errors;
  if output_web then
    Lint_web.generate_web_files
      Format.err_formatter
      master_config
      !file_config_dep
      path
      Lint_db.DefaultDB.db
      Lint_db.DefaultDB.db_errors;
  Lint_text.summary
    master_config
    !file_config_dep
    severity
    path
    pdetail
    no_db
    Lint_db.DefaultDB.db
    Lint_db.DefaultDB.db_errors;
  if not perror && not pwarning && not pdetail then
    Printf.eprintf
      "\n Use --pwarning to display the warnings\n Use --perror to display \
the errors\n Use --pall to display all details\n%!";
  clean_tmp_cfg !file_config_dep master_config;
  if no_db then clean_db_in_tmp db_dir

(* let fork_exec file = *)
(*   let exe = Sys.executable_name in *)
(*   let args = [| exe; "--file"; file |] in *)
(*   let pid = *)
(*     Unix.create_process exe args Unix.stdin Unix.stdout Unix.stderr in *)
(*   match pid with *)
(*   | 0 -> *)
(*     flush stdout; *)
(*     flush stderr *)
(*   | n -> *)
(*     flush stdout; *)
(*     flush stderr *)

(* let lint_parallel path = *)
(*   (\* We filter the global ignored modules/files.  *\) *)
(*   let sources = filter_modules (scan_project path) !!ignored_files in *)
(*   List.iter fork_exec sources *)

(* let scan ?output_text no_db print_only_new path = *)
(*   (\* We filter plugins by using the .ocplint config file and/or *)
(*      command line arguments. *\) *)

(*   let plugins = filter_plugins Lint_globals.plugins in *)

(*   (\* We filter the global ignored modules/files.  *\) *)
(*   let all = filter_modules (scan_project path) !!ignored_files in *)

(*   let no_db = init_db false all path in *)

(*   (\* All inputs for each analyze *\) *)
(*   let mls = List.filter is_source all in *)
(*   let mlis = List.filter is_interface all in *)

(*   let cmts = *)
(*     let files = List.filter is_cmt all in *)
(*     List.map (fun file -> file, lazy (Cmt_format.read_cmt file)) files in *)

(*   let asts_ml, asts_mli = *)
(*     List.map (fun file -> file, lazy (parse_source file)) mls, *)
(*     List.map (fun file -> file, lazy (parse_interf file)) mlis in *)

(*   Format.printf "Starting analyses...\n%!"; *)

(*   Lint_parallel_loop.lint all mls mlis asts_ml asts_mli cmts plugins; *)

(*   Format.printf "Printing results...\n%!"; *)

(*   Lint_text.summary path Lint_db.DefaultDB.db; *)
(*   (\* TODO: do we want to print in stderr by default ? *\) *)
(*   begin match output_text with *)
(*     | None -> print path print_only_new *)
(*     | Some file -> to_text path file *)
(*   end; *)

(*   if not no_db then Lint_db.DefaultDB.save () *)
