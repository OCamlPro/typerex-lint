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

type file_info = {
  file_name : string;
  file_hash : Digest.t;
  file_lines_count : int;
}

type plugin_info = {
  plugin_name : string;
  plugin_description : string;
}

type linter_info = {
  linter_plugin : plugin_info;
  linter_name : string;
  linter_description : string;
}

type warning_info = {
  warning_id : int;
  warning_file : file_info;
  warning_linter : linter_info;
  warning_type : Lint_warning_types.warning;
}

type error_info = {
  error_file : file_info;
  error_type : Lint_db_types.error;
}

type analysis_info = {
  files_info : file_info list;
  plugins_info : plugin_info list;
  linters_info : linter_info list;
  warnings_info : warning_info list;
  errors_info : error_info list;
}

val generated_static_page_of_file :
  file_info ->
  string

val json_of_warnings_info :
  warning_info list ->
  Yojson.Basic.json

val warnings_info_of_json :
  Yojson.Basic.json ->
  warning_info list

val json_of_analysis_info :
  analysis_info ->
  Yojson.Basic.json
    
val analysis_info_of_json :
  Yojson.Basic.json ->
  analysis_info
