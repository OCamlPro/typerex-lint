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
  file_name : string;
  hash : Digest.t;
  plugin_name : string;
  linter_name : string;
  linter_version : string;
  (* option / source *)
  warning_result : Lint_warning_types.warning;
}
				
(** 
todo
 **)	
val json_of_warning :
  warn:Lint_warning_types.warning ->
  Yojson.Basic.json
				
				
(** 
 Convert a database to his JSON representation
 db : the database to convert
 **)	
val json_of_db :
  db:Lint_db_types.t ->
  Yojson.Basic.json

(** 
 Convert a JSON reprensentation of the database to concrete type
 json : the JSON to convert
 **)
val db_of_json :
  json:Yojson.Basic.json ->
  Lint_db_types.t option

(** 
 Convert unsafely a JSON reprensentation of the database to concrete type
 json : the JSON to convert
 raise Type_error or Undefined if the json is not a valid representation of a database
 **)
val unsafe_db_of_json :
  json:Yojson.Basic.json ->
  Lint_db_types.t

(** 
todo
 **)
val raw_entries :
  db:Lint_db_types.t ->
  database_warning_entry list
