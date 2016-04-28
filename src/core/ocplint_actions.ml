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
  let tool_name = Ast_mapper.tool_name () in
  try
    Some
      (Pparse.parse_implementation ~tool_name Format.err_formatter  source)
  with Syntaxerr.Error _ ->
    Printf.printf "Cannot lint %S.\n" source;
    None

let parse_interf source =
  let tool_name = Ast_mapper.tool_name () in
  try
    Some (Pparse.parse_interface ~tool_name Format.err_formatter source)
  with Syntaxerr.Error _ ->
    Printf.printf "Cannont lint %S.\n" source;
    None

let is_source file = Filename.check_suffix file "ml"
let is_interface file = Filename.check_suffix file "mli"
let is_cmt file = Filename.check_suffix file "cmt"
let is_cmt file = Filename.check_suffix file "cmt"


let register_default_sempatch () =
  (* TODO: Fabrice: vérifier que le fichier existe, sinon prendre celui dans
     l'exécutable par défaut*)
  let default_patches = [
    "./src/analysis/plugins/sempatch.md"
  ] in
  let
    module Default = Plugin_sempatch.SempatchPlugin.MakeLintPatch(struct
      let name = "Lint from semantic patches (default)"
      let short_name = "sempatch-lint"
      let details = "Lint from semantic patches (default)."
      let patches = default_patches
    end) in
  ()

let register_default_plugins patches =
  register_default_sempatch ();
  let
    module UserDefined = Plugin_sempatch.SempatchPlugin.MakeLintPatch(struct
      let name = "Lint from semantic patches (user defined)."
      let short_name = "sempatch-lint"
      let details = "Lint from semantic patches (user defined)."
      let patches = patches
    end) in
  ()

let scan ~filters path patches =
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

  register_default_plugins patches;
  Parallel_engine.lint all mls mlis asts_ml asts_mli cmts
