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

let ignored_files = Lint_globals.Config.create_option
    ["ignored_files"]
    "Module to ignore during the lint."
    "Module to ignore during the lint."
    0
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let scan_project path = (* todo *)
  Format.printf "Scanning files in project %S...\n%!" path;
  let found_files =
    let files = ref [] in
    Lint_utils.iter_files (fun file ->
        files := (Filename.concat path file) :: !files) path;
    !files in
  Format.printf "Found '%d' file(s)\n%!" (List.length found_files);
  found_files

let filter_plugins plugins =
  let activated_plugins = Hashtbl.create 42 in
  Lint_plugin.iter_plugins (fun plugin checks ->
      let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
      let plugin_short_name = Plugin.short_name in
      let plugin_opt_names = plugin_short_name :: [ "enabled" ] in
      let plugin_opt_value =
        Lint_globals.Config.get_option_value plugin_opt_names in
      (* if the plugin is disable, don't try to add any linter attached to it *)
      if (bool_of_string plugin_opt_value) then begin
        Lint_map.iter (fun cname lint ->
            let lint_opt_names = plugin_short_name :: cname :: [ "enabled" ] in
            let lint_opt_value =
              Lint_globals.Config.get_option_value lint_opt_names in
            (* if the linter is disable, don't try to use it. *)
            if (bool_of_string lint_opt_value) then begin
              let old_lints =
                try Hashtbl.find activated_plugins plugin
                with Not_found -> Lint_map.empty in
              Hashtbl.replace activated_plugins plugin
                (Lint_map.add cname lint old_lints)
            end)
          checks
      end
    )
    plugins;
  activated_plugins

let filter_modules sources ignore_files =
  List.filter (fun source ->
      not (List.exists (fun ignored -> ignored = source) ignore_files)) sources

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

let rec load_plugins list =
  List.iter (fun file ->
      try
        if Sys.is_directory file then begin
          let files = ref [] in
          Lint_utils.iter_files (fun f ->
              files := (file // f) :: !files) file;
          load_plugins (List.filter is_cmxs !files)
        end
        else if Filename.check_suffix file "cmxs" then
          Dynlink.loadfile file
        else
          Printf.eprintf "Cannot load %S\n%!" file
      with _ ->
        Printf.eprintf "%S: No such file or directory.\n%!" file)
    list

let init_olint_dir () = File.RawIO.safe_mkdir Lint_globals.olint_dirname

let init_db no_db path =
  let path_t = File.of_string path in
  let olint_dirname = Lint_globals.olint_dirname in
  try
    if not no_db then
      (let root_path_dir_t = Lint_utils.find_root path_t [olint_dirname] in
       let root_t =
         File.concat root_path_dir_t (File.of_string olint_dirname) in
       Lint_db.DefaultDB.init root_t);
    no_db
  with Not_found ->
    Printf.eprintf
      "No olint dir found, you should use --init to use DB features.\n%!";
    true

let init_config path =
  let path_t = File.of_string path in
  let config_file = Lint_globals.config_file in
  try
    let root_path_t = Lint_utils.find_root path_t [config_file] in
    let file_t = File.concat root_path_t (File.of_string config_file) in
    Lint_globals.Config.init_config file_t;
  with Not_found -> ()

let output path print_only_new fmt =
  if print_only_new then
    Lint_text.print_only_new fmt path Lint_db.DefaultDB.db
  else Lint_text.print fmt path Lint_db.DefaultDB.db

let print path print_only_new =
  output path print_only_new Format.err_formatter

let to_text path file =
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  output path false fmt;
  close_out oc

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

let get_ignored_files pname cname =
  let opt =
    Lint_globals.Config.create_option [pname; cname; "ignored_files"]  "" "" 0
      (SimpleConfig.list_option SimpleConfig.string_option)  [] in
  !!opt

let is_in_ignored_files file files =
  List.exists (fun source -> source = file) files

let from_input file pname cname inputs =
  List.iter (fun input ->
      try
        match input with
        | Lint_input.InMl main when is_source file ->
          Lint_db.DefaultDB.add_entry file pname cname;
          main file
        | Lint_input.InStruct main when is_source file ->
          begin match parse_source file with
            | Some ast ->
              Lint_db.DefaultDB.add_entry file pname cname;
              main ast
            | None -> assert false
          end
        | Lint_input.InMli main when is_interface file ->
          Lint_db.DefaultDB.add_entry file pname cname;
          main file
        | Lint_input.InInterf main when is_interface file ->
          begin match parse_interf file with
            | Some ast ->
              Lint_db.DefaultDB.add_entry file pname cname;
              main ast
            | None -> ()
          end
        | Lint_input.InCmt main when is_cmt file ->
          Lint_db.DefaultDB.add_entry file pname cname;
          main (Cmt_format.read_cmt file)
        | Lint_input.InAll main ->
          Lint_db.DefaultDB.add_entry file pname cname;
          main [file]
        | Lint_input.InTop main -> assert false (* TODO *)
        | Lint_input.InTokens main when is_source file || is_interface file ->
          begin try
              main (Lexer_iter.get_tokens file)
            with exn -> ()
          end
        | _ -> ()
      with
      | Lint_db_error.Db_error err ->
        Lint_db.DefaultDB.add_error file (Lint_db_types.Db_error err)
      | Sempatch.Failure.SempatchException e ->
        Lint_db.DefaultDB.add_error file (Lint_db_types.Sempatch_error e)
      | Lint_plugin_error.Plugin_error err ->
        Lint_db.DefaultDB.add_error file (Lint_db_types.Plugin_error err)
      | exn ->
        Lint_db.DefaultDB.add_error
          file (Lint_db_types.Ocplint_error (Printexc.to_string exn)))
    inputs

let lint_file no_db file =
  let no_db = init_db no_db file in
  if not no_db then
    (Lint_db.DefaultDB.load_file file;
     Lint_db.DefaultDB.cache ());
  let plugins = filter_plugins Lint_globals.plugins in
  let open Lint_warning_decl in
  let open Lint_warning_types in
  Lint_plugin.iter_plugins (fun plugin checks ->
      Lint_map.iter (fun cname lint ->
          let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
          let module Linter = (val lint : Lint_types.LINT) in
          let ignored_files = get_ignored_files Plugin.short_name cname in
          if not (is_in_ignored_files file ignored_files) &&
             not (Lint_db.DefaultDB.already_run
                    file Plugin.short_name Linter.short_name) then
            from_input file Plugin.short_name Linter.short_name Linter.inputs)
        checks)
    plugins;
  if not no_db then Lint_db.DefaultDB.save ()
  else
    begin
      Lint_text.print
        Format.err_formatter (Filename.dirname file) Lint_db.DefaultDB.db;
      Lint_text.print_error
        Format.err_formatter file Lint_db.DefaultDB.db_errors
    end

let run no_db file =
  (* needed for summary before real parallelization *)
  if no_db then lint_file no_db file
  else begin
    let args = Array.copy Sys.argv in
    let found = ref false in
    Array.iteri (fun i arg ->
        if arg = "--path" then begin
          found := true;
          args.(i) <- "--file";
          args.(i + 1) <- file
        end)
      args;
    let args = (* if ocp-lint is called without --path argument *)
      if not !found then Array.to_list args @ ["--file"; file]
      else Array.to_list args in
    let cmd = String.concat " " args in
    ignore (Sys.command cmd)
  end

let lint_sequential no_db path =
  (* We filter the global ignored modules/files.  *)
  let no_db = init_db no_db path in
  let sources = filter_modules (scan_project path) !!ignored_files in
  List.iter (run no_db) sources;
  let no_db = init_db no_db path in
  if not no_db then
    begin
      Lint_db.DefaultDB.merge sources;
      Lint_text.print Format.err_formatter path Lint_db.DefaultDB.db;
      Lint_text.print_error
        Format.err_formatter path Lint_db.DefaultDB.db_errors
    end;
  Lint_text.summary path Lint_db.DefaultDB.db Lint_db.DefaultDB.db_errors

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
