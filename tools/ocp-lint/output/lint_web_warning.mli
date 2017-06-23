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

type database_warning_entry = {
  warning_id : int;
  warning_file_name : string;
  warning_hash : Digest.t;
  warning_file_lines_count : int;
  warning_plugin_name : string;
  warning_linter_name : string;
  warning_linter_version : string;
  (* option / source *)
  warning_result : Lint_warning_types.warning;
}

val warning_entries_group_by:
  (database_warning_entry -> 'a) ->
  database_warning_entry list ->
  ('a * database_warning_entry list) list
				
val json_of_database_warning_entries :
  database_warning_entry list ->
  Yojson.Basic.json

val database_warning_entries_of_json :
  Yojson.Basic.json ->
  database_warning_entry list
