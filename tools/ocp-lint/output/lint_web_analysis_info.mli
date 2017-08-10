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

(**
  The file informations required by the web interface
  **)
type file_info = {
  (** The name of the file **)
  file_name : string;
  (** The hash of the file **)
  file_hash : Digest.t;
  (** The number line of the file **)
  file_lines_count : int;
}

(**
  The plugin informations required by the web interface
  **)
type plugin_info = {
  (** The name of the plugin **)
  plugin_name : string;
  (** The description of the plugin **)
  plugin_description : string;
}

(**
  The linter informations required by the web interface
  **)
type linter_info = {
  (** The plugin of the linter **)
  linter_plugin : plugin_info;
  (** The name of the linter **)
  linter_name : string;
  (** The description of the linter **)
  linter_description : string;
}

(**
  The error informations required by the web interface
  **)
type warning_info = {
  (** The unique identifier of the warning information **)
  warning_id : int;
  (** The file that raise the warning **)
  warning_file : file_info;
  (** The linter of the warning **)
  warning_linter : linter_info;
  (** The additionals informations (short_name, output, ...) of the warning **)
  warning_type : Lint_warning_types.warning;
}

(**
  The error informations required by the web interface
  **)
type error_info = {
  (** The unique identifier of the error information **)
  error_id : int;
  (** The file that raise the error **)
  error_file : file_info;
  (** The type of the error **)
  error_type : Lint_db_types.error;
}

(**
  All the analysis informations required by the web interface
  **)
type analysis_info = {
  (** The list of the analyzed files **)
  files_info : file_info list;
  (** The list of the enabled plugins **)
  plugins_info : plugin_info list;
  (** The list of the enabled linters **)
  linters_info : linter_info list;
  (** The list of the raised warnings **)
  warnings_info : warning_info list;
  (** The list of the raised errors **)
  errors_info : error_info list;
  (** The root directory of the analysis **)
  analysis_root : string;
  (** The date of the analysis launching **)
  analysis_date : Unix.tm;
}

(**
  Get the name of the generated static file page
  **)
val generated_static_page_of_file : (* todo maybe move in lint_web *)
  file_info ->
  string

(* todo add raised exception to json fun *)

(**
  Convert a list of warning informations to this JSON representation
  **)
val json_of_warnings_info :
  warning_info list ->
  Yojson.Basic.json

(**
  Convert a JSON object to a list of warning information
  **)
val warnings_info_of_json :
  Yojson.Basic.json ->
  warning_info list

(**
  Convert an analysis informations to his JSON representation
  **)
val json_of_analysis_info :
  analysis_info ->
  Yojson.Basic.json

(**
  Convert a JSON object to an analysis informations
  **)
val analysis_info_of_json :
  Yojson.Basic.json ->
  analysis_info
