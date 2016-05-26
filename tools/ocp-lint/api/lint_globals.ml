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

module Config = Lint_config.DefaultConfig

let plugins = Hashtbl.create 42

let olint_dirname = "_olint"
let config_file = ".ocplint"

let default_patches =
  (* To add a static file, edit src/kernel/services/plugins/build.ocp *)
  List.map (fun (file, content) ->
      let tmp = Filename.get_temp_dir_name () in
      let file = Filename.basename file in
      let destfile = Filename.concat tmp file in
      File.Dir.make_all (File.of_string @@ Filename.dirname destfile);
      File.file_of_string destfile content;
      destfile)
    Global_static_files.files

let init_config file =
  SimpleConfig.set_config_file Config.config_file file;
  SimpleConfig.load Config.config_file

let init no_db path =
  let path_t = File.of_string path in
  (try
     let root_path_t = Lint_utils.find_root path_t [config_file] in
     let file_t = File.concat root_path_t (File.of_string config_file) in
     init_config file_t;
   with Not_found -> ());
  try
    if not no_db then
      let root_path_dir_t = Lint_utils.find_root path_t [olint_dirname] in
      let root_t =
        File.concat root_path_dir_t (File.of_string olint_dirname) in
      Lint_db.DefaultDB.init root_t
  with Not_found ->
    Printf.eprintf
      "No DB file found, you should use --init option to use DB features.\n%!";
    exit 1
