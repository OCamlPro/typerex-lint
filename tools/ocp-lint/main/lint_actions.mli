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

(* [load_plugins ()] load dynamically plugins installed
   in lib/ocp-lint-plugins directory. *)
val load_installed_plugins : unit -> string list

(* [load_plugins files] load dynamically files or files in a specific
    directory given in the command line with '--load' option. *)
val load_plugins : string list -> string list

(* [list_plugins fmt] print the list of loaded plugins with their options. *)
val list_plugins : Format.formatter -> unit

(* [init_olint_dir ()] create a temp dir to store the results.
   Used when not root dir is found. This dir is erase at the end of the run. *)
val init_olint_dir : unit -> unit

(* [init_config file] init the config.
   Search for a root dir and a config file to be loaded. *)
val init_config : string -> unit

(* [init_config_file file] load a specific config_file. *)
val init_config_file : string -> unit

(* [init_db ()] create dir to dump the db. *)
val init_db : bool -> string option -> string -> string option * bool

(* [print_db path] debug function to print the db in path. *)
val print_db : string -> unit

(* [lint_file verbose no_db db_dir severity file] lint one file *)
val lint_file :
  verbose:bool ->
  no_db:bool ->
  db_dir:string option ->
  severity:int ->
  file_struct:Lint_utils.file_struct ->
  unit

(* [lint_sequential no_db db_dir severity pdetail pwarning perror
   gd_plugins master_config path]
   no_bd: if we found a root dir for the database
   db_dir: path to the db
   severity: print warnings higher than this severity
   pdetails: verbose
   pwarning: print warnings
   perror: print errors
   gd_plugins: list of plugins that where successfully dynlinked
   ins_plugins: list of installed  plugins that where successfully dynlinked
   master_config: path to a saved master config
   path: starting point of the search for files to analyze
   Iter on files in the path and start ocp-lint process for each file. *)
val lint_sequential :
  no_db:bool ->
  db_dir:string option ->
  severity:int ->
  pdetail:bool ->
  pwarning:bool ->
  perror:bool ->
  gd_plugins:string list ->
  ins_plugins:string list ->
  master_config:string ->
  path:string ->
  unit
